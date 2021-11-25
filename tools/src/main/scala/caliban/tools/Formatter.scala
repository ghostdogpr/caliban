package caliban.tools

import org.scalafmt.dynamic.ConsoleScalafmtReporter
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

      val scalafmt =
        Scalafmt
          .create(this.getClass.getClassLoader)
          .withReporter(new ConsoleScalafmtReporter(System.out)) // defaults prints everything to System.err

      val result = strs.map { case (name, code) => name -> scalafmt.format(config, Paths.get(s"$name.scala"), code) }
      scalafmt.clear()
      result
    }.retryN(3) // We have to retry because of the bug detailed here: https://github.com/scalameta/scalafmt/issues/2793

}
