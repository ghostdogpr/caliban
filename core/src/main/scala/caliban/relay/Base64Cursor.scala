package caliban.relay

import caliban.schema.Schema
import caliban.Value
import scala.util.Try

/**
 * A cursor implementation that models an index/offset as an
 * opaque base64 cursor.
 */
case class Base64Cursor(value: Int)

object Base64Cursor {
  import java.util.Base64
  lazy val encoder = Base64.getEncoder()
  lazy val decoder = Base64.getDecoder()

  private val prefix = "cursor:"

  implicit val cursor: Cursor[Base64Cursor] = new Cursor[Base64Cursor] {
    type T = Int
    def encode(a: Base64Cursor): String =
      encoder.encodeToString(s"$prefix${a.value}".getBytes("UTF-8"))

    def decode(raw: String): Either[String, Base64Cursor] =
      Try({
        val bytes = decoder.decode(raw)
        val s     = new String(bytes, "UTF-8")
        if (s.startsWith(prefix)) {
          Base64Cursor(s.replaceFirst(prefix, "").toInt)
        } else {
          throw new Throwable("invalid cursor")
        }
      }).toEither.left.map(_.getMessage())

    def value(cursor: Base64Cursor): Int = cursor.value
  }

  implicit val schema: Schema[Any, Base64Cursor] =
    Schema.stringSchema.contramap(Cursor[Base64Cursor].encode)
}
