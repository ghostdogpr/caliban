package example.client

import example.client.Client._
import caliban.client.CalibanClientError
import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.client3._
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio.Console.printLine
import zio._

object ExampleApp extends ZIOAppDefault {

  sealed trait Role
  object Role {
    case class Captain(shipName: String)  extends Role
    case class Pilot(shipName: String)    extends Role
    case class Engineer(shipName: String) extends Role
    case class Mechanic(shipName: String) extends Role
  }

  case class Character(name: String, nicknames: List[String], origin: Origin, role: Option[Role])

  override def run = {
    val character = {
      import example.client.Client.Character._
      (name ~
        nicknames ~
        origin ~
        role(
          Captain.shipName.map(Role.Captain),
          Engineer.shipName.map(Role.Engineer),
          Mechanic.shipName.map(Role.Mechanic),
          Pilot.shipName.map(Role.Pilot)
        )).mapN(Character)
    }
    val query     =
      Queries.characters(None) {
        character
      } ~
        Queries.character("Amos Burton") {
          character
        } ~
        Queries.character("Naomi Nagata") {
          character
        } ~
        Queries.character("Alex Kamal") {
          character
        }
    val mutation  = Mutations.deleteCharacter("James Holden")

    def sendRequest[T](
      req: Request[Either[CalibanClientError, T], Any]
    ): RIO[SttpBackend[Task, ZioStreams with WebSockets], T] =
      ZIO
        .serviceWithZIO[SttpBackend[Task, ZioStreams with WebSockets]](req.send(_))
        .map(_.body)
        .absolve
        .tap(res => printLine(s"Result: $res"))

    val uri   = uri"http://localhost:8088/api/graphql"
    val call1 = sendRequest(mutation.toRequest(uri))
    val call2 = sendRequest(query.toRequest(uri, useVariables = true))

    (call1 *> call2)
      .provideLayer(HttpClientZioBackend.layer())
      .exitCode
  }
}
