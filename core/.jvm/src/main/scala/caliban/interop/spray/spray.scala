package caliban.interop.spray

import caliban.introspection.adt.__Type
import caliban.schema.Step.QueryStep
import caliban.schema.Types.makeScalar
import caliban.schema.{ ArgBuilder, PureStep, Schema, Step }
import caliban.{ InputValue, ResponseValue }
import spray.json.{ JsValue, JsonReader, JsonWriter }
import zio.ZIO
import zquery.ZQuery

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark spray-json dependency as optional and keep Readers defined in the companion object.
 */
private[caliban] trait IsSprayJsonReader[F[_]]
private[caliban] object IsSprayJsonReader {
  implicit val isSprayJsonReader: IsSprayJsonReader[JsonReader] = null
}

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark spray-json dependency as optional and keep Writers defined in the companion object.
 */
private[caliban] trait IsSprayJsonWriter[F[_]]
private[caliban] object IsSprayJsonWriter {
  implicit val isSprayJsonWriter: IsSprayJsonWriter[JsonWriter] = null
}

object json {
  implicit val jsonSchema: Schema[Any, JsValue] = new Schema[Any, JsValue] {
    override def toType(isInput: Boolean): __Type = makeScalar("Json")
    override def resolve(value: JsValue): Step[Any] =
      QueryStep(ZQuery.fromEffect(ZIO.effect(implicitly[JsonReader[ResponseValue]].read(value))).map(PureStep))
  }
  implicit val jsonArgBuilder: ArgBuilder[JsValue] = (input: InputValue) =>
    Right(implicitly[JsonWriter[InputValue]].write(input))
}
