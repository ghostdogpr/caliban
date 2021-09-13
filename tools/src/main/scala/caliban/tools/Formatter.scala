package caliban.tools

import org.scalafmt.interfaces.Scalafmt
import zio.RIO
import zio.blocking.{ effectBlocking, Blocking }

import java.nio.file.{ Files, Path, Paths }

object Formatter {

  def format(str: String, fmtPath: Option[String]): RIO[Blocking, String]                                  =
    format(List("Nil.scala" -> str), fmtPath).map(_.head._2)

  def format(strs: List[(String, String)], fmtPath: Option[String]): RIO[Blocking, List[(String, String)]] =
    effectBlocking {
      val config: Path = {
        @inline def defaultConfigPath = Paths.get(".scalafmt.conf")
        @inline def defaultConfig     = if (Files.exists(defaultConfigPath)) defaultConfigPath else Paths.get("")

        fmtPath.fold(defaultConfig)(Paths.get(_))
      }

      val scalafmt = Scalafmt.create(this.getClass.getClassLoader).withDefaultVersion("2.7.5")
      val result   = strs.map { case (name, code) =>
        name -> scalafmt.format(config, Paths.get(s"$name.scala"), code)
      }
      scalafmt.clear()
      result
    }

}
