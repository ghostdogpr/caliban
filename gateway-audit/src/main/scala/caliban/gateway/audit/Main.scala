package caliban.gateway.audit

import caliban.QuickAdapter
import caliban.gateway.{ Gateway, GatewayInterpreter, Subgraph }
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromArray, JsonValueCodec }
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import zio._
import zio.http._

import scala.util.Try

object Main extends ZIOAppDefault {

  private val DefaultAuditUrl = "http://127.0.0.1:4200"
  private val GatewayPort     = 4000

  private[audit] final case class SubgraphInput(name: String, url: String, sdl: String)
  private implicit val subgraphInputsCodec: JsonValueCodec[List[SubgraphInput]] = JsonCodecMaker.make

  override def run: ZIO[ZIOAppArgs, Throwable, Unit] =
    program
      .provideSome[ZIOAppArgs](Client.default)
      .tapErrorCause(cause => ZIO.logErrorCause("Federation audit adapter failed.", cause))

  private val program =
    ZIO.scoped {
      for {
        args        <- ZIOAppArgs.getArgs
        suite       <- ZIO
                         .fromOption(args.headOption)
                         .orElseFail(new IllegalArgumentException("Expected one Federation audit suite id."))
        auditUrl    <- System.envOrElse("FEDERATION_GATEWAY_AUDIT_URL", DefaultAuditUrl)
        inputs      <- fetchSubgraphs(auditUrl, suite)
        subgraphs   <- ZIO.foreach(inputs)(toSubgraph)
        interpreter <- buildInterpreter(subgraphs)
        _           <- ZIO.logInfo(s"Serving Federation audit suite '$suite' on port $GatewayPort.")
        _           <- serve(interpreter)
      } yield ()
    }

  private def fetchSubgraphs(auditUrl: String, suite: String): ZIO[Client, Throwable, List[SubgraphInput]] =
    for {
      endpoint <- ZIO
                    .fromEither(URL.decode(s"$auditUrl/$suite/subgraphs"))
                    .mapError(error => new IllegalArgumentException(error.getMessage, error))
      response <- Client.batched(Request.get(endpoint))
      _        <- ZIO
                    .fail(new IllegalStateException(s"Audit fixture request failed with HTTP ${response.status.code}."))
                    .unless(response.status.isSuccess)
      body     <- response.body.asArray
      inputs   <- ZIO.fromEither(decodeSubgraphs(body).left.map(new IllegalArgumentException(_)))
    } yield inputs

  private def toSubgraph(input: SubgraphInput): Task[Subgraph[Any]] =
    ZIO
      .fromEither(URL.decode(input.url))
      .mapError(error => new IllegalArgumentException(s"Invalid endpoint for '${input.name}'.", error))
      .map(endpoint => Subgraph.federation(input.name, endpoint, input.sdl))

  private def buildInterpreter(subgraphs: List[Subgraph[Any]]): ZIO[Scope, Throwable, GatewayInterpreter[Any]] =
    subgraphs match {
      case first :: rest =>
        Gateway
          .compose(first, rest: _*)
          .interpreter
          .mapError(error => new IllegalArgumentException(error.diagnostics.mkString(" ")))
      case Nil           => ZIO.fail(new IllegalArgumentException("Audit fixture returned no subgraphs."))
    }

  private def serve(interpreter: GatewayInterpreter[Any]): ZIO[Any, Throwable, Nothing] =
    Server
      .serve(
        QuickAdapter(interpreter).routes("/graphql") ++ Routes(Method.GET / "health" -> Handler.ok)
      )
      .provide(Server.defaultWithPort(GatewayPort))

  private[audit] def decodeSubgraphs(bytes: Array[Byte]): Either[String, List[SubgraphInput]] =
    Try(readFromArray[List[SubgraphInput]](bytes)).toEither.left.map(_ =>
      "Audit fixture subgraphs were not valid JSON."
    )
}
