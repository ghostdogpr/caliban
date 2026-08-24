package caliban.execution

import caliban.CalibanError.ValidationError
import caliban.Configurator.ExecutionConfiguration
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

  private[caliban] def prepareParsed(
    request: GraphQLRequest,
    document: Document,
    variables: Map[String, InputValue],
    rootType: RootType,
    skipValidation: Boolean
  )(implicit trace: Trace): IO[ValidationError, ExecutionRequest] =
    Configurator.ref.getWith { config =>
      prepareParsed(request, document, variables, rootType, config, skipValidation, config.validations)
    }

  private[caliban] def prepareParsed(
    request: GraphQLRequest,
    document: Document,
    variables: Map[String, InputValue],
    rootType: RootType,
    skipValidation: Boolean,
    validations: List[Validator.QueryValidation]
  )(implicit trace: Trace): IO[ValidationError, ExecutionRequest] =
    Configurator.ref.getWith { config =>
      prepareParsed(request, document, variables, rootType, config, skipValidation, validations)
    }

  private def prepareParsed(
    request: GraphQLRequest,
    document: Document,
    variables: Map[String, InputValue],
    rootType: RootType,
    config: ExecutionConfiguration,
    skipValidation: Boolean,
    validations: List[Validator.QueryValidation]
  )(implicit trace: Trace): IO[ValidationError, ExecutionRequest] =
    Validator
      .prepare(
        document,
        rootType,
        request.operationName,
        variables,
        config.skipValidation || skipValidation,
        validations
      )
      .fold(
        Exit.fail,
        execution => checkIntrospection(config)(execution) *> checkHttpMethod(config)(request, execution)
      )

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
