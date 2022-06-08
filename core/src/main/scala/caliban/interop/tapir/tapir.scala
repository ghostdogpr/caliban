package caliban.interop.tapir

import caliban._
import sttp.tapir.{ Schema, SchemaType }

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark circe dependency as optional and keep Encoders defined in the companion object.
 */
private[caliban] trait IsTapirSchema[F[_]]
private[caliban] object IsTapirSchema {
  implicit val isCirceEncoder: IsTapirSchema[Schema] = null
}

object schema {
  implicit lazy val requestSchema: Schema[GraphQLRequest]      =
    sttp.tapir.Schema[GraphQLRequest](SchemaType.SString[GraphQLRequest]())
  implicit def responseSchema[E]: Schema[GraphQLResponse[E]]   =
    sttp.tapir.Schema[GraphQLResponse[E]](SchemaType.SString[GraphQLResponse[E]]())
  implicit lazy val wsInputSchema: Schema[GraphQLWSInput]      =
    sttp.tapir.Schema[GraphQLWSInput](SchemaType.SString[GraphQLWSInput]())
  implicit lazy val wsOutputSchema: Schema[GraphQLWSOutput]    =
    sttp.tapir.Schema[GraphQLWSOutput](SchemaType.SString[GraphQLWSOutput]())
  implicit lazy val responseValueSchema: Schema[ResponseValue] =
    sttp.tapir.Schema[ResponseValue](SchemaType.SString[ResponseValue]())
}
