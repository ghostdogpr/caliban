package caliban.gateway.audit

import caliban.QuickAdapter
import caliban.gateway.{ Gateway, GatewayRuntime, Subgraph }
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromArray, JsonValueCodec }
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import sttp.client4.{ asByteArrayAlways, basicRequest }
import sttp.client4.httpclient.zio.{ HttpClientZioBackend, SttpClient }
import sttp.model.Uri
import zio._
import zio.http._

import scala.util.Try

object Main extends ZIOAppDefault {

  private val DefaultAuditUrl = "http://127.0.0.1:4200"
  private val GatewayPort     = 4000

  private[audit] final case class SubgraphInput(name: String, url: String, sdl: String)
  private implicit val subgraphInputsCodec: JsonValueCodec[List[SubgraphInput]] = JsonCodecMaker.make

  override def run =
    program.tapErrorCause(cause => ZIO.logErrorCause("Federation audit adapter failed.", cause))

  private val program =
    ZIO.scoped {
      for {
        args      <- ZIOAppArgs.getArgs
        suite     <- ZIO
                       .fromOption(args.headOption)
                       .orElseFail(new IllegalArgumentException("Expected one Federation audit suite id."))
        auditUrl  <- System.envOrElse("FEDERATION_GATEWAY_AUDIT_URL", DefaultAuditUrl)
        backend   <- HttpClientZioBackend.scoped()
        inputs    <- fetchSubgraphs(auditUrl, suite, backend)
        subgraphs <- ZIO.foreach(inputs)(toSubgraph)
        runtime   <- buildRuntime(subgraphs)
        _         <- ZIO.logInfo(s"Serving Federation audit suite '$suite' on port $GatewayPort.")
        _         <- serve(runtime)
      } yield ()
    }

  private def fetchSubgraphs(auditUrl: String, suite: String, backend: SttpClient): Task[List[SubgraphInput]] =
    for {
      endpoint <- ZIO
                    .fromEither(Uri.parse(s"$auditUrl/$suite/subgraphs"))
                    .mapError(error => new IllegalArgumentException(error))
      response <- basicRequest.get(endpoint).response(asByteArrayAlways).send(backend)
      _        <- ZIO
                    .fail(new IllegalStateException(s"Audit fixture request failed with HTTP ${response.code.code}."))
                    .unless(response.code.isSuccess)
      inputs   <- ZIO.fromEither(decodeSubgraphs(response.body).left.map(new IllegalArgumentException(_)))
    } yield inputs

  private def toSubgraph(input: SubgraphInput): Task[Subgraph[Any]] =
    ZIO
      .fromEither(Uri.parse(input.url))
      .mapError(error => new IllegalArgumentException(s"Invalid endpoint for '${input.name}': $error"))
      .map(endpoint => Subgraph.federation(input.name, endpoint, input.sdl))

  private def buildRuntime(subgraphs: List[Subgraph[Any]]): ZIO[Scope, Throwable, GatewayRuntime[Any]] =
    subgraphs match {
      case first :: rest =>
        Gateway
          .compose(first, rest: _*)
          .build
          .mapError(error => new IllegalArgumentException(error.diagnostics.mkString(" ")))
      case Nil           => ZIO.fail(new IllegalArgumentException("Audit fixture returned no subgraphs."))
    }

  private def serve(runtime: GatewayRuntime[Any]): ZIO[Any, Throwable, Nothing] =
    Server
      .serve(
        QuickAdapter(runtime).routes("/graphql") ++ Routes(Method.GET / "health" -> Handler.ok)
      )
      .provide(Server.defaultWithPort(GatewayPort))

  private[audit] def decodeSubgraphs(bytes: Array[Byte]): Either[String, List[SubgraphInput]] =
    Try(readFromArray[List[SubgraphInput]](bytes)).toEither.left.map(_ =>
      "Audit fixture subgraphs were not valid JSON."
    )
}
