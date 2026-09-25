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
 * Parsing, variable coercion and validation steps shared by interpreters that execute a validated request themselves.
 */
private[caliban] object RequestPreparation {

  def parse(query: String): IO[CalibanError.ParsingError, Document] =
    Exit.fromEither(Parser.parseQuery(query))

  def coerceVariables(document: Document, request: GraphQLRequest, rootType: RootType)(implicit
    trace: Trace
  ): IO[ValidationError, Map[String, InputValue]] =
    Configurator.ref.getWith { config =>
      if (isIntrospectionDisabled(config, document, request.operationName)) introspectionDisabled
      else
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

  def prepareParsed(
    request: GraphQLRequest,
    document: Document,
    variables: Map[String, InputValue],
    rootType: RootType,
    skipValidation: Boolean,
    validations: Option[List[Validator.QueryValidation]] = None
  )(implicit trace: Trace): IO[ValidationError, ExecutionRequest] =
    Configurator.ref.getWith { config =>
      Validator.prepare(
        document,
        rootType,
        request.operationName,
        variables,
        config.skipValidation || skipValidation,
        validations.getOrElse(config.validations)
      ) match {
        case Right(execution) => checkHttpMethod(config, request, execution)
        case Left(error)      => Exit.fail(error)
      }
    }

  def checkIntrospection(document: Document, operationName: Option[String])(implicit
    trace: Trace
  ): IO[ValidationError, Unit] =
    Configurator.ref.getWith { config =>
      if (isIntrospectionDisabled(config, document, operationName)) introspectionDisabled
      else Exit.unit
    }

  private def introspectionDisabled: IO[ValidationError, Nothing] =
    Exit.fail(CalibanError.ValidationError("Introspection is disabled", ""))

  private def isIntrospectionDisabled(
    config: ExecutionConfiguration,
    document: Document,
    operationName: Option[String]
  ): Boolean =
    !config.enableIntrospection && Introspector.hasIntrospection(document, operationName)

  private def checkHttpMethod(
    config: ExecutionConfiguration,
    request: GraphQLRequest,
    execution: ExecutionRequest
  ): IO[ValidationError, ExecutionRequest] =
    if (
      execution.operationType == OperationType.Mutation &&
      !config.allowMutationsOverGetRequests &&
      request.isHttpGetRequest
    ) Exit.fail(HttpUtils.MutationOverGetError)
    else Exit.succeed(execution)
}
