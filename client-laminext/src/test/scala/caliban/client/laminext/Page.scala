package caliban.client.laminext

import com.raquo.laminar.api.L._
import io.laminext.syntax.core._
import io.laminext.websocket.WebSocket

object Page {
  private val characters: Var[List[String]] = Var(Nil)

  private val uri = "http://localhost:8088/api/graphql"
  private val ws  = WebSocket.url("ws://localhost:8088/ws/graphql", "graphql-ws").graphql.build()

  private val getCharacters                 = Client.Queries.characters(None)(Client.Character.name).toEventStream(uri)
  private def deleteCharacter(name: String) = Client.Mutations.deleteCharacter(name).toEventStream(uri)
  private val deletedCharacters             = Client.Subscriptions.characterDeleted.toSubscription(ws)

  val view: Div =
    div(
      "Characters: ",
      getCharacters.collectRight --> characters.set _,
      ws.connect,
      ws.connected --> (_ => ws.init()),
      deletedCharacters.received.collectRight --> (name => characters.update(_.filterNot(_ == name))),
      child <-- characters.signal.map(c => div(c.mkString(", "))),
      br(),
      button(
        "Delete first",
        disabled <-- characters.signal.map(_.isEmpty),
        thisEvents(onClick).flatMap(_ =>
          characters.now().headOption match {
            case Some(name) => deleteCharacter(name)
            case None       => EventStream.empty
          }
        ) --> (_ => ())
      )
    )
}
