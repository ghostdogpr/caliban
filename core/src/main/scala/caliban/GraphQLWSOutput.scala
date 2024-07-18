package caliban

import caliban.interop.tapir.IsTapirSchema
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

case class GraphQLWSOutput(`type`: String, id: Option[String], payload: Option[ResponseValue])

object GraphQLWSOutput {
  private[caliban] implicit val jsoniterCodec: JsonValueCodec[GraphQLWSOutput] = JsonCodecMaker.make

  implicit def tapirSchema[F[_]: IsTapirSchema]: F[GraphQLWSOutput] =
    caliban.interop.tapir.schema.wsOutputSchema.asInstanceOf[F[GraphQLWSOutput]]
}
