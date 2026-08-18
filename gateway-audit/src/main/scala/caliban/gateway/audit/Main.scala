package caliban.gateway.audit

import caliban.ResponseValue._
import caliban.Value.StringValue
import caliban.gateway.{ Gateway, GatewayRuntime, Subgraph }
import caliban.{ GraphQLRequest, ResponseValue }
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromArray, writeToArray }
import sttp.client4.{ asByteArrayAlways, basicRequest }
import sttp.client4.httpclient.zio.{ HttpClientZioBackend, SttpClient }
import sttp.model.Uri
import zio._
import zio.http._

import scala.util.control.NonFatal

object Main extends ZIOAppDefault {

  private val DefaultAuditUrl = "http://127.0.0.1:4200"
  private val GatewayPort     = 4000

  private[audit] final case class SubgraphInput(name: String, url: String, sdl: String)

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

  private def serve(runtime: GatewayRuntime[Any]): ZIO[Any, Throwable, Nothing] = {
    val graphql = Handler.fromFunctionZIO[Request] { request =>
      (for {
        bytes    <- request.body.asArray
        decoded  <- ZIO.attempt(readFromArray[GraphQLRequest](bytes))
        response <- runtime.executeRequest(decoded)
      } yield Response(
        Status.Ok,
        Headers(Header.Custom("Content-Type", "application/graphql-response+json")),
        Body.fromArray(writeToArray(response))
      )).catchAll(error =>
        ZIO.succeed(
          Response(Status.BadRequest, body = Body.fromString(s"Invalid GraphQL request: ${error.getMessage}"))
        )
      )
    }

    Server
      .serve(
        Routes(
          Method.GET / "health"   -> Handler.ok,
          Method.POST / "graphql" -> graphql
        )
      )
      .provide(Server.defaultWithPort(GatewayPort))
  }

  private[audit] def decodeSubgraphs(bytes: Array[Byte]): Either[String, List[SubgraphInput]] =
    try
      readFromArray[ResponseValue](bytes) match {
        case ListValue(values) =>
          values
            .foldLeft[Either[String, List[SubgraphInput]]](Right(Nil)) { (result, value) =>
              for {
                inputs <- result
                input  <- decodeSubgraph(value)
              } yield input :: inputs
            }
            .map(_.reverse)
        case _                 => Left("Audit fixture subgraphs must be a JSON array.")
      }
    catch {
      case NonFatal(_) => Left("Audit fixture subgraphs were not valid JSON.")
    }

  private def decodeSubgraph(value: ResponseValue): Either[String, SubgraphInput] =
    value match {
      case ObjectValue(fields) =>
        for {
          name <- stringField(fields, "name")
          url  <- stringField(fields, "url")
          sdl  <- stringField(fields, "sdl")
        } yield SubgraphInput(name, url, sdl)
      case _                   => Left("Each audit fixture subgraph must be a JSON object.")
    }

  private def stringField(fields: List[(String, ResponseValue)], name: String): Either[String, String] =
    fields.collectFirst { case (`name`, StringValue(value)) => value }.toRight(s"Missing string field '$name'.")
}
