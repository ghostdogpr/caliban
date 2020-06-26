package caliban.interop.play

import caliban.GraphQLRequest
import caliban.interop.RequestDecoderSuite
import zio.test.environment.TestEnvironment
import zio.test._
import play.api.libs.json._

object GraphQLRequestPlaySpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    RequestDecoderSuite("GraphQLRequestPlaySpec")(s => Json.parse(s).validate[GraphQLRequest].get)
}
