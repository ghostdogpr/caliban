package caliban.parsing

import caliban.TestUtils
import org.apache.commons.lang3.SerializationUtils
import zio._
import zio.test._

object DocumentSpec extends ZIOSpecDefault {
  def spec = suite("DocumentSpec")(
    test("is serializable") {
      for {
        doc1 <- Parser.parseQuery(TestUtils.introspectionQuery)
        doc2 <- ZIO.attempt(SerializationUtils.roundtrip(doc1))
      } yield assertTrue(doc1 == doc2)
    }
  )
}
