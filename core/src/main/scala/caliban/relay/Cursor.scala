package caliban.relay

/**
 * A trait representing an abstract Relay Connection cursor.
 */
trait Cursor[A] {
  type T
  def encode(a: A): String
  def decode(s: String): Either[String, A]
  def value(cursor: A): T
}

object Cursor {
  def apply[A](implicit c: Cursor[A]): Cursor[A] = c
}
