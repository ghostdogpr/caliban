package caliban.interop.jsoniter

import caliban.GraphQLRequest._
import caliban.interop.RequestDecoderSuite
import zio.test.environment.TestEnvironment
import zio.test._
import com.github.plokhotnyuk.jsoniter_scala.core._

object GraphQLRequestJsoniterSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    RequestDecoderSuite("GraphQLRequestJsoniterSpec")(s => readFromArray(s.getBytes("UTF-8")))
}
