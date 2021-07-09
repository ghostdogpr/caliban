package caliban.tools

import java.nio.file.{ Files, Paths }

import org.scalafmt.interfaces.Scalafmt
import zio.Task

object Formatter {

  def format(str: String, fmtPath: Option[String]): Task[String]                                  =
    format(List("Nil.scala" -> str), fmtPath).map(_.head._2)

  def format(strs: List[(String, String)], fmtPath: Option[String]): Task[List[(String, String)]] = Task {
    val scalafmt          = Scalafmt.create(this.getClass.getClassLoader)
    val defaultConfigPath = Paths.get(".scalafmt.conf")
    val defaultConfig     =
      if (Files.exists(defaultConfigPath)) defaultConfigPath else Paths.get("")
    val config            = fmtPath.fold(defaultConfig)(Paths.get(_))
    val result            = strs.map { case (name, code) =>
      name -> scalafmt
        .withRespectVersion(false)
        .format(config, Paths.get(s"$name.scala"), code)
    }
    scalafmt.clear()
    result
  }

}
