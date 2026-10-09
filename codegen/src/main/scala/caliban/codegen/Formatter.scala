package caliban.codegen

import org.scalafmt.interfaces.Scalafmt
import zio.{ Task, ZIO }

import java.nio.file.{ Files, Path, Paths, StandardCopyOption }
import java.util.jar.JarFile

object Formatter {

  def format(str: String, fmtPath: Option[String]): Task[String] =
    format(List("Nil.scala" -> str), fmtPath).map(_.head._2)

  def format(strs: List[(String, String)], fmtPath: Option[String]): Task[List[(String, String)]] =
    ZIO.attemptBlocking {
      val config: Path = {
        val defaultConfigPath = Paths.get(".scalafmt.conf")
        fmtPath.fold(if (Files.exists(defaultConfigPath)) defaultConfigPath else bundledConfig)(Paths.get(_))
      }

      strs.map { case (name, code) => name -> scalafmt.format(config, Paths.get(s"$name.scala"), code) }
    }.retryN(3) // We have to retry because of the bug detailed here: https://github.com/scalameta/scalafmt/issues/2793

  // Shared: resolving and loading scalafmt takes seconds, and it reloads a config file when that file changes
  private lazy val scalafmt: Scalafmt = buildScalaFmt()

  private lazy val bundledConfig: Path = {
    val defaultScalafmtCalibanToolsFile = "default.scalafmt.conf"
    val uri                             = this.getClass.getClassLoader.getResource(defaultScalafmtCalibanToolsFile).toURI
    uri.getScheme match {
      case "file" => Paths.get(uri)
      case "jar"  =>
        // scalafmt can't access a file inside a JAR so we'll copy the content into a temp file
        val jar            = new JarFile(this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI.getPath)
        val file           = Files.createTempFile(null, null)
        val scalafmtConfig = jar.getInputStream(jar.getEntry(defaultScalafmtCalibanToolsFile))
        Files.copy(scalafmtConfig, file, StandardCopyOption.REPLACE_EXISTING)
        file
      case _      => Paths.get("")
    }
  }

  def buildScalaFmt(): Scalafmt = {
    import coursierapi.{ Dependency, Fetch }
    import org.scalafmt.interfaces.{ RepositoryPackageDownloaderFactory, Scalafmt }

    import java.net.URLClassLoader
    import java.util.ServiceLoader
    import scala.jdk.CollectionConverters._

    val scalaVersion = BuildInfo.scalaPartialVersion match {
      case Some((2, 12)) => "2.12"
      case Some((2, 13)) => "2.13"
      case Some((3, _))  => "2.13"
      case _             => "2.12"
    }

    val files       = Fetch
      .create()
      .addDependencies(Dependency.of("org.scalameta", s"scalafmt-dynamic_$scalaVersion", BuildInfo.scalafmtVersion))
      .fetch()
    val parent      = new ScalafmtBridgeClassLoader(this.getClass.getClassLoader)
    val classLoader = new URLClassLoader(files.asScala.toArray.map(_.toURI().toURL()), parent)
    val factory     = ServiceLoader.load(classOf[RepositoryPackageDownloaderFactory], classLoader).iterator().next()
    Scalafmt.create(classLoader).withRepositoryPackageDownloader(factory)
  }
}

private[codegen] final class ScalafmtBridgeClassLoader(parent: ClassLoader)
    extends ClassLoader(ClassLoader.getPlatformClassLoader) {

  override protected def findClass(name: String): Class[_] =
    if (name.startsWith("org.scalafmt.interfaces")) parent.loadClass(name)
    else super.findClass(name)
}
