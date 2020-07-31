package caliban.client.apq.hash

import java.security.MessageDigest

import Hashing.Hasher
import cats.data.Kleisli

import scala.util.Try

private[hash] trait CryptoHashers {
  private[hash] lazy val _Sha256: Hasher[Array[Byte], Array[Byte]] = JdkHasher.Sha256
}

private[hash] object JdkHasher {
  lazy val Sha256 = apply("SHA-256")

  def apply(algorithm: String): Hasher[Array[Byte], Array[Byte]] =
    Kleisli(bytes =>
      Try(MessageDigest.getInstance(algorithm).digest(bytes)).toEither.left.map(err => s"Cannot get $algorithm hash")
    )
}
