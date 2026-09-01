package caliban.execution

import caliban.CalibanError.ValidationError
import caliban.Configurator.ExecutionConfiguration
import caliban.parsing.adt.{ Document, OperationType, Selection }
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
    skipValidation: Boolean,
    validations: Option[List[Validator.QueryValidation]] = None
  )(implicit trace: Trace): IO[ValidationError, ExecutionRequest] =
    Configurator.ref.getWith { config =>
      prepare(request, document, variables, rootType, config, skipValidation, validations.getOrElse(config.validations))
    }

  private def prepare(
    request: GraphQLRequest,
    document: Document,
    variables: Map[String, InputValue],
    rootType: RootType,
    config: ExecutionConfiguration,
    skipValidation: Boolean,
    validations: List[Validator.QueryValidation]
  )(implicit trace: Trace): IO[ValidationError, ExecutionRequest] =
    checkIntrospection(config, document, request.operationName) *>
      Validator
        .prepare(
          document,
          rootType,
          request.operationName,
          variables,
          config.skipValidation || skipValidation,
          validations
        )
        .fold(Exit.fail, checkHttpMethod(config)(request, _))

  def parse(query: String): IO[CalibanError.ParsingError, Document] =
    Exit.fromEither(Parser.parseQuery(query))

  def coerceVariables(
    document: Document,
    request: GraphQLRequest,
    rootType: RootType
  )(implicit trace: Trace): IO[ValidationError, Map[String, InputValue]] =
    Configurator.ref.getWith { config =>
      checkIntrospection(config, document, request.operationName) *>
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
    config: ExecutionConfiguration,
    document: Document,
    operationName: Option[String]
  ): IO[ValidationError, Unit] =
    if (!config.enableIntrospection && hasIntrospection(document, operationName))
      Exit.fail(CalibanError.ValidationError("Introspection is disabled", ""))
    else Exit.unit

  private def hasIntrospection(document: Document, operationName: Option[String]): Boolean = {
    val fragments = document.fragmentDefinitions.iterator.map(fragment => fragment.name -> fragment).toMap

    def loop(selections: List[Selection], visited: Set[String]): Boolean =
      selections.exists {
        case Selection.Field(_, name, _, _, nested, _) =>
          name == "__schema" || name == "__type" || loop(nested, visited)
        case Selection.InlineFragment(_, _, nested)    => loop(nested, visited)
        case Selection.FragmentSpread(name, _)         =>
          !visited.contains(name) && fragments.get(name).exists(fragment => loop(fragment.selectionSet, visited + name))
      }

    document
      .operationDefinition(operationName)
      .exists(operation => operation.operationType == OperationType.Query && loop(operation.selectionSet, Set.empty))
  }

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
