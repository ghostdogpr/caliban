package caliban

import caliban.interop.tapir.IsTapirSchema
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

case class GraphQLWSInput(`type`: String, id: Option[String], payload: Option[InputValue])

object GraphQLWSInput {
  implicit val jsoniterCodec: JsonValueCodec[GraphQLWSInput] = JsonCodecMaker.make

  implicit def tapirSchema[F[_]: IsTapirSchema]: F[GraphQLWSInput] =
    caliban.interop.tapir.schema.wsInputSchema.asInstanceOf[F[GraphQLWSInput]]
}
