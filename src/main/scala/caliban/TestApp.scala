package caliban

import caliban.parsing.Parser
import fastparse.Parsed

object TestApp extends App {

  val queryString =
    """
      query {
        characters(origin: "MARS") {
          name
          nicknames
          role
        }
        character(name: "Amos Burton") {
          name
          nicknames
          origin
        }
      }
      """

  val Parsed.Success(query, _) = Parser.parseQuery(queryString)

  println(GraphQL.schema[Test.Query])
  println("")
  println(GraphQL.execute(query, Test.resolver).mkString("\n"))

}
