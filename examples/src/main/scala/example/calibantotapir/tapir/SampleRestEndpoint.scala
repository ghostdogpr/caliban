package example.calibantotapir.tapir

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import sttp.capabilities.zio.ZioStreams
import sttp.tapir.Schema
import sttp.tapir.json.jsoniter._
import sttp.tapir.ztapir._
import zio._

object SampleRestEndpoint {
  // NOTE: You can also add the graphiql endpoint in a similar fashion
  val endpoint: ZServerEndpoint[Any, ZioStreams] =
    infallibleEndpoint.get
      .in("example")
      .out(jsonBody[SampleResponse])
      .serverLogic(_ => ZIO.right(SampleResponse("Hello", 1, 2)))

  final case class SampleResponse(data: String, x: Int, y: Int)
  object SampleResponse {
    implicit val sampleResponseCodec: JsonValueCodec[SampleResponse] = JsonCodecMaker.make
    implicit val sampleResponseSchema: Schema[SampleResponse]        = Schema.derived
  }
}
