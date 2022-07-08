package caliban.tools

import org.scalafmt.interfaces.Scalafmt
import zio.RIO
import zio.blocking.{ effectBlocking, Blocking }

import java.nio.file.{ Files, Path, Paths, StandardCopyOption }
import java.util.jar.JarFile

object Formatter {

  def format(str: String, fmtPath: Option[String]): RIO[Blocking, String] =
    format(List("Nil.scala" -> str), fmtPath).map(_.head._2)

  def format(strs: List[(String, String)], fmtPath: Option[String]): RIO[Blocking, List[(String, String)]] =
    effectBlocking {
      val config: Path = {
        @inline def defaultConfigPath = Paths.get(".scalafmt.conf")
        @inline def defaultConfig     =
          if (Files.exists(defaultConfigPath)) defaultConfigPath
          else {
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

        fmtPath.fold(defaultConfig)(Paths.get(_))
      }

      val scalafmt = buildScalaFmt()

      val result = strs.map { case (name, code) => name -> scalafmt.format(config, Paths.get(s"$name.scala"), code) }
      scalafmt.clear()
      result
    }.retryN(3) // We have to retry because of the bug detailed here: https://github.com/scalameta/scalafmt/issues/2793

  def buildScalaFmt(): Scalafmt = {
    import coursierapi.{ Dependency, Fetch, Module }
    import org.scalafmt.interfaces.{ Scalafmt, ScalafmtClassLoader, ScalafmtReporter }

    import java.io.{ File, PrintStream }
    import java.net.URLClassLoader
    import scala.jdk.CollectionConverters._

    val scalaVersion = BuildInfo.scalaPartialVersion match {
      case Some((2, 12)) => "2.12"
      case Some((2, 13)) => "2.13"
      case Some((3, _))  => "2.13"
      case _             => "2.12"
    }

    val files                      = Fetch
      .create()
      .addDependencies(Dependency.of("org.scalameta", s"scalafmt-dynamic_$scalaVersion", BuildInfo.scalafmtVersion))
      .fetch()
    val classLoader                = new URLClassLoader(files.asScala.toArray.map(_.toURI().toURL()), this.getClass.getClassLoader)
    val fmt                        = Scalafmt.create(classLoader)
    val reporterClass              = classLoader.loadClass("org.scalafmt.dynamic.ConsoleScalafmtReporter")
    val constructor                = reporterClass.getConstructor(classOf[PrintStream]);
    val reporter: ScalafmtReporter = constructor.newInstance(System.err).asInstanceOf[ScalafmtReporter]
    fmt.withReporter(reporter)
  }
}
