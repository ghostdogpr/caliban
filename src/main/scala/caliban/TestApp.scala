package caliban

import caliban.parsing.Parser
import caliban.GraphQL._
import fastparse.Parsed
import zio.console.putStrLn
import zio.{ App, Runtime, UIO, ZIO }

object TestApp extends App {

  implicit val runtime: Runtime[Environment] = this

  val queryString =
    """
      query {
        characters(origin: "MARS") {
          name
          nicknames
          role
        }
        amos: character(name: "Amos Burton") {
          name
          nicknames
          origin
        }
      }
      """

  val Parsed.Success(query, _) = Parser.parseQuery(queryString)

  val graph: GraphQL[Test.Query] = graphQL[Test.Query]

  println(graph.render)
  println("")

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    (for {
      result <- graph.execute(query, Test.resolver)
      _      <- putStrLn(result.mkString("\n"))
    } yield ()).foldM(ex => putStrLn(ex.toString).as(1), _ => UIO.succeed(0))
}
