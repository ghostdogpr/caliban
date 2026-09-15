package caliban.parsing

import caliban.TestUtils
import caliban.parsing.adt.Selection
import org.apache.commons.lang3.SerializationUtils
import zio.test._

object DocumentSpec extends ZIOSpecDefault {
  def spec = suite("DocumentSpec")(
    test("is serializable") {
      for {
        doc1 <- Parser.parseQuery(TestUtils.introspectionQuery)
        doc2  = SerializationUtils.roundtrip(doc1)
      } yield assertTrue(doc1 == doc2)
    },
    test("visits shared fragments once") {
      val depth          = 40
      val fragments      = (0 until depth).map { index =>
        val body = if (index == depth - 1) "a { c }" else s"a { ...F${index + 1} } b { ...F${index + 1} }"
        s"fragment F$index on Query { $body }"
      }
      val query          = s"{ a { ...F0 } b { ...F0 } } ${fragments.mkString(" ")}"
      val expectedFields = 2 * depth + 2
      for {
        document <- Parser.parseQuery(query)
        visited   = {
          var count = 0
          document.foreachSelection(None) {
            case _: Selection.Field => count += 1
            case _                  => ()
          }
          count
        }
      } yield assertTrue(visited == expectedFields)
    }
  )
}
