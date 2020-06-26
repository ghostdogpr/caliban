package caliban.interop.circe

import caliban.interop.ResponseEncoderSuite
import zio.test._
import zio.test.environment.TestEnvironment
import io.circe.syntax._

object GraphQLResponseCirceSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    ResponseEncoderSuite("GraphQLResponseCirceSpec")(resp => resp.asJson.toString())
}
