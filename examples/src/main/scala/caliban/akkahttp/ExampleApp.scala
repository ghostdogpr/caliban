package caliban.akkahttp

import akka.NotUsed

import scala.io.StdIn
import akka.actor.{ ActorRef, ActorSystem }
import akka.stream.CompletionStrategy

import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{ BinaryMessage, Message, TextMessage }
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.ws.TextMessage.Streamed

import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{ Flow, Keep, Sink, Source }
import caliban.ExampleData.{ sampleCharacters, Character, CharacterArgs, CharactersArgs, Role }
import caliban.GraphQL.graphQL
import caliban.schema.Annotations.{ GQLDeprecated, GQLDescription }
import caliban.schema.GenericSchema
import caliban.{ AkkaHttpAdapter, ExampleService, RootResolver }
import zio.clock.Clock
import zio.console.Console
import zio.stream.ZStream
import zio.{ DefaultRuntime, URIO }

import scala.concurrent.Future

object ExampleApp extends App with GenericSchema[Console with Clock] {

  implicit val system           = ActorSystem()
  implicit val executionContext = system.dispatcher
  implicit val defaultRuntime   = new DefaultRuntime {}

  implicit val roleSchema           = gen[Role]
  implicit val characterSchema      = gen[Character]
  implicit val characterArgsSchema  = gen[CharacterArgs]
  implicit val charactersArgsSchema = gen[CharactersArgs]

  trait Protocol
  case object Complete           extends Protocol
  case class Fail(ex: Exception) extends Protocol

  def handleMessage(message: String, actor: ActorRef): Unit =
    println(s"handleMessage got ws message '$message' from client")
  // should hook up to AkkaHttpAdapter to act on the subscription

  def createFlow(actor: ActorRef): Sink[Message, NotUsed] =
    Flow[Message].map {
      case TextMessage.Strict(message) =>
        println(s"createFlow got message $message")
        handleMessage(message, actor)
        Nil
      case Streamed(textStream) =>
        textStream.runFold("")(_ + _).map(message => handleMessage(message, actor)).flatMap(Future.successful)
        Nil
      case bm: BinaryMessage =>
        println(s"createFlow got BinaryMessage ")
        // ignore binary messages but drain content to avoid the stream being clogged
        bm.dataStream.runWith(Sink.ignore)
        Nil
    }.to(Sink.ignore)

  def createSource(): (Source[TextMessage.Strict, NotUsed], ActorRef) = {
    println("createSource")
    val completionMatcher: PartialFunction[Any, CompletionStrategy] = {
      case Complete =>
        println("createSource got Complete")
        CompletionStrategy.draining
      case Fail(ex) =>
        println(s"createSource got Fail ${ex}")
        CompletionStrategy.draining
    }
    val failureMatcher: PartialFunction[Any, Throwable] = {
      case Fail(ex) =>
        println(s"createSource failureMatcher ${ex}")
        ex
    }
    val (ref, publisher) =
      Source
        .actorRef[TextMessage.Strict](
          completionMatcher,
          failureMatcher,
          bufferSize = Int.MaxValue,
          overflowStrategy = OverflowStrategy.fail
        )
        .toMat(Sink.asPublisher(true))(Keep.both)
        .run()

    (Source.fromPublisher(publisher), ref)
  }

  case class Queries(
    @GQLDescription("Return all characters from a given origin")
    characters: CharactersArgs => URIO[Console, List[Character]],
    @GQLDeprecated("Use `characters`")
    character: CharacterArgs => URIO[Console, Option[Character]]
  )
  case class Mutations(deleteCharacter: CharacterArgs => URIO[Console, Boolean])
  case class Subscriptions(characterDeleted: ZStream[Console, Nothing, String])

  val service = defaultRuntime.unsafeRun(ExampleService.make(sampleCharacters))

  val interpreter =
    graphQL(
      RootResolver(
        Queries(
          args => service.getCharacters(args.origin),
          args => service.findCharacter(args.name)
        ),
        Mutations(args => service.deleteCharacter(args.name)),
        Subscriptions(service.deletedEvents)
      )
    ).interpreter

  /**
   * curl -X POST \
   * http://localhost:8088/api/graphql \
   * -H 'Host: localhost:8088' \
   * -H 'Content-Type: application/json' \
   * -d '{
   * "query": "query { characters { name }}"
   * }'
   */
  val route =
    path("api" / "graphql") {
      AkkaHttpAdapter.makeHttpService(interpreter)
    } ~ path("graphiql") {
      getFromResource("graphiql.html")
    } ~ path("ws" / "graphql") {
      get {
        val (source, actor)              = createSource()
        val sink: Sink[Message, NotUsed] = createFlow(actor)
        extractUpgradeToWebSocket { upgrade ⇒
          complete(upgrade.handleMessagesWithSinkSource(sink, source, subprotocol = Some("graphql-ws")))
        }
      }
    }

  val bindingFuture = Http().bindAndHandle(route, "localhost", 8088)
  println(s"Server online at http://localhost:8088/\nPress RETURN to stop...")
  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())

}
