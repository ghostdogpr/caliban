package caliban.client.apq.hash

import Hashing.{ Hasher, Result }
import cats.data.Kleisli
import scodec.bits.ByteVector

import scala.scalajs.js.JSConverters._
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.Try

private[hash] trait CryptoHashers {
  private[hash] lazy val _Sha256: Hasher[Array[Byte], Array[Byte]] = JsHasher.Sha256
}

private[hash] object JsHasher {
  def tryFn[A, B](fn: A ⇒ B)(errorText: String): Hasher[A, B] =
    Kleisli(a ⇒ tryUnit(fn(a))(errorText))

  def tryUnit[B](fn: ⇒ B)(errorText: String): Result[B] =
    Try(fn).toEither.left.map(_ ⇒ errorText)

  lazy val Sha256 = tryFn[Array[Byte], Array[Byte]] { msg =>
    val sha256 = new SHA256()
    sha256.update(new Uint8Array(msg.map(_.toShort).toJSArray))
    ByteVector.fromValidHex(sha256.digest("hex")).toArray
  }("Cannot calculate Sha256 hash")
}
