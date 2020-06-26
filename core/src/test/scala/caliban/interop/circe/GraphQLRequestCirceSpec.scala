package caliban.interop.circe

import caliban.GraphQLRequest
import caliban.interop.RequestDecoderSuite
import io.circe.parser._
import zio.test.environment.TestEnvironment
import zio.test._

object GraphQLRequestCirceSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    RequestDecoderSuite("GraphQLRequestCirceSpec")(s => parse(s).toTry.get.as[GraphQLRequest].toTry.get)
}
