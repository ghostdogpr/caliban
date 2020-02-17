package caliban.client

import caliban.client.Autogen.Role._
import caliban.client.Autogen._
import sttp.client._
import sttp.client.asynchttpclient.zio.AsyncHttpClientZioBackend
import zio.console.putStrLn
import zio.{ App, ZIO }

object ExampleApp extends App {

  sealed trait Role
  object Role {
    case class Captain(shipName: String)  extends Role
    case class Pilot(shipName: String)    extends Role
    case class Engineer(shipName: String) extends Role
    case class Mechanic(shipName: String) extends Role
  }

  case class Character(name: String, nicknames: List[String], origin: Origin, role: Option[Role])

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    val character = {
      import caliban.client.Autogen.Character._
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

    AsyncHttpClientZioBackend().flatMap { implicit backend =>
      val uri   = uri"http://localhost:8088/api/graphql"
      val call1 = mutation.toRequest(uri).send().map(_.body).absolve
      val call2 = query.toRequest(uri, useVariables = true).send().map(_.body).absolve
      call1.tap(res => putStrLn(s"Result: $res")) *>
        call2.tap(res => putStrLn(s"Result: $res"))
    }.foldM(ex => putStrLn(ex.toString).as(1), _ => ZIO.succeed(0))
  }

  /*
  TODO
  - scaladoc
  - cleanup examples
  - microsite

  - support interfaces
  - subscriptions (no backend for ScalaJS yet)
  - compact query with fragments
 */

}
