package caliban.reporting

import zio.{ Task, ZIO }

import java.security.MessageDigest

object Util {

  private[caliban] def hashSchema(schema: String): Task[String] =
    ZIO.effect(
      MessageDigest
        .getInstance("SHA-256")
        .digest(schema.getBytes("UTF-8"))
        .map("%02x".format(_))
        .mkString
    )

}
