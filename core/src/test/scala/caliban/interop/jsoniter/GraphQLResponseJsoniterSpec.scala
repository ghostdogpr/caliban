package caliban.interop.jsoniter

import caliban.interop.ResponseEncoderSuite
import zio.test.environment.TestEnvironment
import zio.test.{ DefaultRunnableSpec, ZSpec }
import com.github.plokhotnyuk.jsoniter_scala.core._

object GraphQLResponseJsoniterSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    ResponseEncoderSuite("GraphQLResponseJsoniterSpec")(s => writeToString(s))
}
