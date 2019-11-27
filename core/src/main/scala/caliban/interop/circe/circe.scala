package caliban.interop.circe

import io.circe._

import scala.language.higherKinds

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark circe dependency as optional and keep Encoders defined in the companion object.
 */
private[caliban] trait IsCirceEncoder[F[_]]
private[caliban] object IsCirceEncoder {
  implicit val isCirceEncoder: IsCirceEncoder[Encoder] = null
}

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark circe dependency as optional and keep Decoders defined in the companion object.
 */
private[caliban] trait IsCirceDecoder[F[_]]
private[caliban] object IsCirceDecoder {
  implicit val isCirceDecoder: IsCirceDecoder[Decoder] = null
}
