package caliban.akkahttp

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.Directives._
import caliban.{AkkaHttpAdapter, ExampleService, RootResolver}
import caliban.ExampleApp.{Mutations, Queries, Subscriptions}
import caliban.ExampleData.sampleCharacters
import caliban.GraphQL.graphQL
import zio.DefaultRuntime

import scala.io.StdIn

object ExampleApp extends App {

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  implicit val defaultRuntime = new DefaultRuntime {}

  case class Character(name: String)
  def getCharacters: List[Character] = List(Character("Tobias"), Character("Jonas"), Character("Caliban"))
  def getCharacter(name: String): Option[Character] = getCharacters.find(_.name == name)

  case class CharacterName(name: String)
  case class Queries(characters: List[Character],
                     character: CharacterName => Option[Character])

  val queries = Queries(getCharacters, args => getCharacter(args.name))
  val interpreter = graphQL(RootResolver(queries))


  /**
   * curl -X POST \
   * http://localhost:8080/api/graphql \
   * -H 'Host: localhost:8080' \
   * -H 'Content-Type: application/json' \
   * -d '{
   * "query": "query { characters { name }}"
   * }'
   */

  val route =
      path("api" / "graphql") {
        AkkaHttpAdapter.makeRestService(interpreter)
      }

  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())

}
