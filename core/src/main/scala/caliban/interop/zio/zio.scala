package caliban.interop.zio

import zio.json.{ JsonDecoder, JsonEncoder }

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark circe dependency as optional and keep Encoders defined in the companion object.
 */
private[caliban] trait IsZIOJsonEncoder[F[_]]
private[caliban] object IsZIOJsonEncoder {
  implicit val isZIOJsonEncoder: IsZIOJsonEncoder[JsonEncoder] = null
}

private[caliban] trait IsZIOJsonDecoder[F[_]]
private[caliban] object IsZIOJsonDecoder {
  implicit val isZIOJsonDecoder: IsZIOJsonDecoder[JsonDecoder] = null
}
