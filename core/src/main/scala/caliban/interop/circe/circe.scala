package caliban.interop.circe

import caliban.introspection.adt.__Type
import caliban.schema.Step.QueryStep
import caliban.schema.Types.makeScalar
import caliban.schema.{ ArgBuilder, PureStep, Schema, Step }
import caliban.{ InputValue, ResponseValue }
import io.circe._
import zio.ZIO
import zquery.ZQuery

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

object json {
  implicit val jsonSchema: Schema[Any, Json] = new Schema[Any, Json] {
    override def toType(isInput: Boolean): __Type = makeScalar("Json")
    override def resolve(value: Json): Step[Any] =
      QueryStep(ZQuery.fromEffect(ZIO.fromEither(Decoder[ResponseValue].decodeJson(value))).map(PureStep))
  }
  implicit val jsonArgBuilder: ArgBuilder[Json] = (input: InputValue) => Right(Encoder[InputValue].apply(input))
}
