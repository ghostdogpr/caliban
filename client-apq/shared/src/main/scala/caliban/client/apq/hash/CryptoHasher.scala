package caliban.client.apq.hash

import scodec.bits.ByteVector

object CryptoHasher extends CryptoHashers {
  def Sha256(bytes: Array[Byte]): Either[String, String] =
    (_Sha256(bytes): Either[String, Array[Byte]]).right.map {
      ByteVector(_).toHex
    }
}
