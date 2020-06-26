package caliban.interop.play

import caliban.GraphQLResponse
import caliban.interop.ResponseEncoderSuite
import zio.test._
import zio.test.environment.TestEnvironment
import play.api.libs.json._

object GraphQLResponsePlaySpec extends DefaultRunnableSpec {

  val writer: Writes[GraphQLResponse[Any]] = implicitly[Writes[GraphQLResponse[Any]]]

  override def spec: ZSpec[TestEnvironment, Any] =
    ResponseEncoderSuite("GraphQLResponsePlaySpec")(resp => writer.writes(resp).toString())
}
