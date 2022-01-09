package caliban.tools

import zio.RIO
import zio.blocking.{ effectBlocking, Blocking }

import java.nio.file.{ Files, Path, Paths, StandardCopyOption }
import java.util.jar.JarFile

object Formatter {

  def format(str: String, fmtPath: Option[String]): RIO[Blocking, String] =
    format(List("Nil.scala" -> str), fmtPath).map(_.head._2)

  def format(strs: List[(String, String)], fmtPath: Option[String]): RIO[Blocking, List[(String, String)]] =
    effectBlocking {
      strs
    }.retryN(3) // We have to retry because of the bug detailed here: https://github.com/scalameta/scalafmt/issues/2793

}
