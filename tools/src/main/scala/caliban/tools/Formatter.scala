package caliban.tools

import java.nio.file.{ Files, Paths }

import org.scalafmt.interfaces.Scalafmt
import zio.Task

object Formatter {

  def format(str: String, fmtPath: Option[String]): Task[String] = Task {
    val scalafmt          = Scalafmt.create(this.getClass.getClassLoader)
    val defaultConfigPath = Paths.get(".scalafmt.conf")
    val defaultConfig     =
      if (Files.exists(defaultConfigPath)) defaultConfigPath else Paths.get("")
    val config            = fmtPath.fold(defaultConfig)(Paths.get(_))
    val result            = scalafmt
      .withRespectVersion(false)
      .format(config, Paths.get("Nil.scala"), str)
    scalafmt.clear()
    result
  }

}
