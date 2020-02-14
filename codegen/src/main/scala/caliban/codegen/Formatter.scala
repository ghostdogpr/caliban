package caliban.codegen

import java.net.{ URL, URLClassLoader }
import java.nio.file.Paths
import org.scalafmt.dynamic.ScalafmtReflect
import org.scalafmt.dynamic.utils.ReentrantCache
import org.scalafmt.interfaces.Scalafmt
import zio.Task

object Formatter {

  def format(str: String, fmtPath: String): Task[String] = Task {
    val scalafmt = Scalafmt.create(this.getClass.getClassLoader)
    val config   = Paths.get(fmtPath)
    scalafmt.format(config, Paths.get("Nil.scala"), str)
  }

  def formatStr(code: String, fmt: String): Task[String] = Task {
    ReentrantCache()
    val scalafmtReflect =
      ScalafmtReflect(
        new URLClassLoader(new Array[URL](0), this.getClass.getClassLoader),
        "2.3.2",
        respectVersion = false
      )
    val config = scalafmtReflect.parseConfigFromString(fmt)

    scalafmtReflect.format(code, config)
  }

}
