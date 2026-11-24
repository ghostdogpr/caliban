package caliban.parsing

import caliban.introspection.Introspector
import caliban.parsing.adt.Selection
import zio._
import zio.test._

object DocumentTraversalSpec extends ZIOSpecDefault {
  def spec = suite("DocumentSpec - traversal")(
    test("visits shared fragments once") {
      val depth          = 40
      val fragments      = (0 until depth).map { index =>
        val body = if (index == depth - 1) "a { c }" else s"a { ...F${index + 1} } b { ...F${index + 1} }"
        s"fragment F$index on Query { $body }"
      }
      val query          = s"{ a { ...F0 } b { ...F0 } } ${fragments.mkString(" ")}"
      val expectedFields = 2 * depth + 2
      for {
        document <- ZIO.fromEither(Parser.parseQuery(query))
        visited  <- ZIO.succeed {
                      var count = 0
                      document
                        .operationDefinition(None)
                        .foreach(document.existsSelection(_) { selection =>
                          selection match {
                            case _: Selection.Field => count += 1
                            case _                  => ()
                          }
                          false
                        })
                      (count, Introspector.hasIntrospection(document, None))
                    }.disconnect
      } yield assertTrue(visited == ((expectedFields, false)))
    } @@ TestAspect.timeout(10.seconds)
  )
}
