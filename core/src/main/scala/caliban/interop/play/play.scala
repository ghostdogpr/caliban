package caliban.interop.play

import caliban.introspection.adt.__Type
import caliban.schema.Step.QueryStep
import caliban.schema.Types.makeScalar
import caliban.schema.{ ArgBuilder, PureStep, Schema, Step }
import caliban.{ InputValue, ResponseValue }
import play.api.libs.json.{ JsPath, JsValue, Json, JsonValidationError, Reads, Writes }
import zio.ZIO
import zquery.ZQuery

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark play-json dependency as optional and keep Writes defined in the companion object.
 */
private[caliban] trait IsPlayJsonWrites[F[_]]
private[caliban] object IsPlayJsonWrites {
  implicit val isPlayJsonWrites: IsPlayJsonWrites[Writes] = null
}

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark play-json dependency as optional and keep Reads defined in the companion object.
 */
private[caliban] trait IsPlayJsonReads[F[_]]
private[caliban] object IsPlayJsonReads {
  implicit val isPlayJsonReads: IsPlayJsonReads[Reads] = null
}

object json {
  implicit val jsonSchema: Schema[Any, JsValue] = new Schema[Any, JsValue] {
    private def parse(value: JsValue) =
      implicitly[Reads[ResponseValue]]
        .reads(value)
        .asEither
        .left
        .map(parsingException)

    override def toType(isInput: Boolean): __Type = makeScalar("Json")
    override def resolve(value: JsValue): Step[Any] =
      QueryStep(ZQuery.fromEffect(ZIO.fromEither(parse(value))).map(PureStep))
  }
  implicit val jsonArgBuilder: ArgBuilder[JsValue] = (input: InputValue) => Right(Json.toJson(input))

  private[caliban] def parsingException(errs: Seq[(JsPath, Seq[JsonValidationError])]) =
    new Throwable(s"Couldn't decode json: $errs")
}
