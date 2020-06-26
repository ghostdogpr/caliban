package caliban.interop.jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core._

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark jsoniter dependency as optional and keep Codecs defined in the companion object.
 */
private[caliban] trait IsJsoniterCodec[F[_]]
private[caliban] object IsJsoniterCodec {
  implicit val isJsoniterCodec: IsJsoniterCodec[JsonValueCodec] = null
}

private[caliban] abstract class JsonValueEncoder[@specialized A] extends JsonValueCodec[A] {
  override def decodeValue(in: JsonReader, default: A): A =
    throw new RuntimeException("decoding not implemented")
  override def nullValue: A =
    throw new RuntimeException("nullValue not implemented")
}

private[caliban] abstract class JsonValueDecoder[@specialized A] extends JsonValueCodec[A] {
  override def encodeValue(x: A, out: JsonWriter): Unit =
    throw new RuntimeException("encoding not implemented")
}
