package caliban

import caliban.GraphQL._
import zio.console.putStrLn
import zio.{ App, Runtime, UIO, ZIO }

object TestApp extends App {
  implicit val runtime: Runtime[Environment] = this

  val query =
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

  val graph: GraphQL[Test.Query] = graphQL[Test.Query]

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] =
    (for {
      _      <- putStrLn(graph.render + "\n")
      result <- graph.execute(query, Test.resolver)
      _      <- putStrLn(result.mkString("\n"))
    } yield ()).foldM(ex => putStrLn(ex.toString).as(1), _ => UIO.succeed(0))
}
