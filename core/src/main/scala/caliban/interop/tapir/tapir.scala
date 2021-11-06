package caliban.interop.tapir

import caliban._
import caliban.ResponseValue.StreamValue
import caliban.Value.NullValue
import sttp.tapir.generic.auto._
import sttp.tapir.Schema
import zio.stream.ZStream

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark circe dependency as optional and keep Encoders defined in the companion object.
 */
private[caliban] trait IsTapirSchema[F[_]]
private[caliban] object IsTapirSchema {
  implicit val isCirceEncoder: IsTapirSchema[Schema] = null
}

object schema {
  implicit val streamSchema: Schema[StreamValue]               =
    Schema.schemaForUnit.map(_ => Some(StreamValue(ZStream(NullValue))))(_ => ())
  implicit val throwableSchema: Schema[Throwable]              = Schema.string
  implicit lazy val inputValueSchema: Schema[InputValue]       = Schema.derivedSchema
  implicit lazy val responseValueSchema: Schema[ResponseValue] = Schema.derivedSchema
  implicit val calibanErrorSchema: Schema[CalibanError]        = Schema.derivedSchema
  implicit val requestSchema: Schema[GraphQLRequest]           = Schema.derivedSchema
  implicit def responseSchema[E]: Schema[GraphQLResponse[E]]   = {
    implicit val schemaE: Schema[E] = Schema.string
    Schema.derivedSchema[GraphQLResponse[E]]
  }
  implicit val wsInputSchema: Schema[GraphQLWSInput]           = Schema.derivedSchema
  implicit val wsOutputSchema: Schema[GraphQLWSOutput]         = Schema.derivedSchema
}
