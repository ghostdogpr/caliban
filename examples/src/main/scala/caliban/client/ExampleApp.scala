package caliban.client

import caliban.client.Client._
import sttp.client._
import sttp.client.asynchttpclient.zio.{ AsyncHttpClientZioBackend, SttpClient }
import zio.console.{ putStrLn, Console }
import zio.{ App, ExitCode, RIO, ZIO }

object ExampleApp extends App {

  sealed trait Role
  object Role {
    case class Captain(shipName: String)  extends Role
    case class Pilot(shipName: String)    extends Role
    case class Engineer(shipName: String) extends Role
    case class Mechanic(shipName: String) extends Role
  }

  case class Character(name: String, nicknames: List[String], origin: Origin, role: Option[Role])

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, ExitCode] = {
    val character = {
      import caliban.client.Client.Character._
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
    val query =
      Queries.characters(None) {
        character
      } ~
        Queries.character("Amos Burton") {
          character
        } ~
        Queries.character("Naomi Nagata") {
          character
        }
    val mutation = Mutations.deleteCharacter("James Holden")

    def send[T](req: Request[Either[CalibanClientError, T], Nothing]): RIO[Console with SttpClient, T] =
      SttpClient.send(req).map(_.body).absolve.tap(res => putStrLn(s"Result: $res"))

    val uri   = uri"http://localhost:8088/api/graphql"
    val call1 = send(mutation.toRequest(uri))
    val call2 = send(query.toRequest(uri, useVariables = true))

    (call1 *> call2)
      .provideCustomLayer(AsyncHttpClientZioBackend.layer())
      .foldM(ex => putStrLn(ex.toString).as(ExitCode.failure), _ => ZIO.succeed(ExitCode.success))
  }
}
