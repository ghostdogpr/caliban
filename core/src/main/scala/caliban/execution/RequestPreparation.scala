package caliban.execution

import caliban.CalibanError.ValidationError
import caliban.Configurator.ExecutionConfiguration
import caliban.introspection.Introspector
import caliban.parsing.adt.{ Document, OperationType }
import caliban.parsing.{ Parser, VariablesCoercer }
import caliban.schema.RootType
import caliban.validation.Validator
import caliban.{ CalibanError, Configurator, GraphQLRequest, HttpUtils, InputValue }
import zio.{ Exit, IO, Trace }

/**
 * The shared Caliban operation front-end used by interpreters that execute an already validated request themselves.
 */
private[caliban] object RequestPreparation {

  final case class Prepared(document: Document, executionRequest: ExecutionRequest)

  def check(query: String, rootType: RootType)(implicit trace: Trace): IO[CalibanError, Unit] =
    for {
      document <- parse(query)
      _        <- Validator.validate(document, validationRoot(rootType))
    } yield ()

  def prepare(request: GraphQLRequest, rootType: RootType)(implicit trace: Trace): IO[CalibanError, Prepared] =
    for {
      document  <- parse(request.query.getOrElse(""))
      root       = validationRoot(rootType)
      variables <- coerceVariables(document, request, root)
      execution <- validate(document, request, variables, root)
    } yield Prepared(document, execution)

  def parse(query: String): IO[CalibanError.ParsingError, Document] =
    Exit.fromEither(Parser.parseQuery(query))

  def coerceVariables(
    document: Document,
    request: GraphQLRequest,
    rootType: RootType
  )(implicit trace: Trace): IO[ValidationError, Map[String, InputValue]] =
    Configurator.ref.getWith { config =>
      Exit.fromEither(
        VariablesCoercer.coerceVariables(
          request.variables.getOrElse(Map.empty),
          document,
          rootType,
          config.skipValidation,
          request.operationName
        )
      )
    }

  def validate(
    document: Document,
    request: GraphQLRequest,
    variables: Map[String, InputValue],
    rootType: RootType
  )(implicit trace: Trace): IO[ValidationError, ExecutionRequest] =
    Configurator.ref.getWith { config =>
      Validator
        .prepare(
          document,
          rootType,
          request.operationName,
          variables,
          config.skipValidation,
          config.validations
        )
        .fold(
          Exit.fail,
          execution => checkIntrospection(config)(execution) *> checkHttpMethod(config)(request, execution)
        )
    }

  private def validationRoot(rootType: RootType): RootType =
    Introspector.withIntrospection(rootType)

  private def checkIntrospection(
    config: ExecutionConfiguration
  )(execution: ExecutionRequest): IO[ValidationError, Unit] =
    if (!config.enableIntrospection && execution.hasIntrospection)
      Exit.fail(CalibanError.ValidationError("Introspection is disabled", ""))
    else Exit.unit

  private def checkHttpMethod(
    config: ExecutionConfiguration
  )(request: GraphQLRequest, execution: ExecutionRequest): IO[ValidationError, ExecutionRequest] =
    if (
      execution.operationType == OperationType.Mutation &&
      !config.allowMutationsOverGetRequests &&
      request.isHttpGetRequest
    ) Exit.fail(HttpUtils.MutationOverGetError)
    else Exit.succeed(execution)
}
