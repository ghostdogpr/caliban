package caliban.tools

import org.scalafmt.interfaces.Scalafmt
import zio.RIO
import zio.blocking.{ effectBlocking, Blocking }
import zio.prelude._

import java.nio.file.{ Files, Path, Paths }

object Formatter {

  def format(str: String, fmtPath: Option[String]): RIO[Blocking, String] =
    format(List("Nil.scala" -> str), fmtPath).map(_.head._2)

  def format(strs: List[(String, String)], fmtPath: Option[String]): RIO[Blocking, List[(String, String)]] = {
    def configuration: RIO[Blocking, Path] =
      effectBlocking {
        @inline def defaultConfigPath = Paths.get(".scalafmt.conf")
        @inline def defaultConfig     = if (Files.exists(defaultConfigPath)) defaultConfigPath else Paths.get("")

        fmtPath.fold(defaultConfig)(Paths.get(_))
      }

    for {
      scalafmt <- effectBlocking(Scalafmt.create(this.getClass.getClassLoader))
      config   <- configuration
      result   <- strs.forEach { case (name, code) =>
                    effectBlocking {
                      name -> scalafmt
                        .withRespectVersion(false)
                        .format(config, Paths.get(s"$name.scala"), code)
                    }
                  }
      _        <- effectBlocking(scalafmt.clear())
    } yield result
  }

}
