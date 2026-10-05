package caliban.gateway

/**
 * A finding reported while composing subgraph schemas into the client schema.
 *
 * Errors fail the build with [[GatewayBuildError.SchemaCompositionFailed]], which also reports the warnings found
 * so far. Warnings are logged when an interpreter is built, and fail the build too when
 * [[GatewayConfig.withFatalCompositionWarnings]] is enabled. `subgraphs` names the subgraphs involved, sorted, and
 * `coordinate` locates the schema element when the finding has one.
 */
final case class CompositionDiagnostic(
  severity: CompositionDiagnostic.Severity,
  code: CompositionDiagnostic.Code,
  subgraphs: List[String],
  coordinate: Option[SchemaCoordinate],
  message: String
) {
  def render: String = s"[${if (subgraphs.isEmpty) "composition" else subgraphs.mkString(", ")}] $message"
}

object CompositionDiagnostic {

  private[gateway] def error(code: Code, subgraphs: Iterable[String], coordinate: Option[SchemaCoordinate])(
    message: String
  ): CompositionDiagnostic =
    CompositionDiagnostic(Severity.Error, code, subgraphs.toList.distinct.sorted, coordinate, message)

  sealed trait Severity

  object Severity {
    case object Error   extends Severity
    case object Warning extends Severity
  }

  /**
   * Identifies the rule a diagnostic reports. Apollo Federation's composition error codes are used where one
   * applies; the other codes are specific to Caliban.
   */
  sealed abstract class Code(val name: String) {
    override def toString: String = name
  }

  object Code {
    case object ContextInvalidSelection                 extends Code("CONTEXT_INVALID_SELECTION")
    case object ContextNameInvalid                      extends Code("CONTEXT_NAME_INVALID")
    case object ContextNotSet                           extends Code("CONTEXT_NOT_SET")
    case object ContextualArgumentNotContextualInAllSubgraphs
        extends Code("CONTEXTUAL_ARGUMENT_NOT_CONTEXTUAL_IN_ALL_SUBGRAPHS")
    case object CostAppliedToInterfaceField             extends Code("COST_APPLIED_TO_INTERFACE_FIELD")
    case object DirectiveCompositionError               extends Code("DIRECTIVE_COMPOSITION_ERROR")
    case object EnumValueMismatch                       extends Code("ENUM_VALUE_MISMATCH")
    case object FieldTypeMismatch                       extends Code("FIELD_TYPE_MISMATCH")
    case object InvalidFieldSharing                     extends Code("INVALID_FIELD_SHARING")
    case object InvalidGraphQL                          extends Code("INVALID_GRAPHQL")
    case object InvalidLinkDirectiveUsage               extends Code("INVALID_LINK_DIRECTIVE_USAGE")
    case object InvalidLookup                           extends Code("INVALID_LOOKUP")
    case object KeyInvalidFields                        extends Code("KEY_INVALID_FIELDS")
    case object ListSizeAppliedToNonList                extends Code("LIST_SIZE_APPLIED_TO_NON_LIST")
    case object ListSizeInvalidAssumedSize              extends Code("LIST_SIZE_INVALID_ASSUMED_SIZE")
    case object ListSizeInvalidSizedField               extends Code("LIST_SIZE_INVALID_SIZED_FIELD")
    case object ListSizeInvalidSlicingArgument          extends Code("LIST_SIZE_INVALID_SLICING_ARGUMENT")
    case object MissingTransitiveAuthRequirements       extends Code("MISSING_TRANSITIVE_AUTH_REQUIREMENTS")
    case object NoContextInSelection                    extends Code("NO_CONTEXT_IN_SELECTION")
    case object NoQueries                               extends Code("NO_QUERIES")
    case object OnlyInaccessibleChildren                extends Code("ONLY_INACCESSIBLE_CHILDREN")
    case object OverrideCollisionWithAnotherDirective   extends Code("OVERRIDE_COLLISION_WITH_ANOTHER_DIRECTIVE")
    case object OverrideFromSelfError                   extends Code("OVERRIDE_FROM_SELF_ERROR")
    case object OverrideLabelInvalid                    extends Code("OVERRIDE_LABEL_INVALID")
    case object OverrideOnInterface                     extends Code("OVERRIDE_ON_INTERFACE")
    case object OverrideSourceHasOverride               extends Code("OVERRIDE_SOURCE_HAS_OVERRIDE")
    case object ProvidesInvalidFields                   extends Code("PROVIDES_INVALID_FIELDS")
    case object QueryRootTypeInaccessible               extends Code("QUERY_ROOT_TYPE_INACCESSIBLE")
    case object ReferencedInaccessible                  extends Code("REFERENCED_INACCESSIBLE")
    case object RequiredInaccessible                    extends Code("REQUIRED_INACCESSIBLE")
    case object RequiredInputFieldMissingInSomeSubgraph extends Code("REQUIRED_INPUT_FIELD_MISSING_IN_SOME_SUBGRAPH")
    case object RequiresInvalidFields                   extends Code("REQUIRES_INVALID_FIELDS")
    case object ScalarDefinitionMismatch                extends Code("SCALAR_DEFINITION_MISMATCH")
    case object TypeKindMismatch                        extends Code("TYPE_KIND_MISMATCH")
    case object UnenforceableSecurityDirective          extends Code("UNENFORCEABLE_SECURITY_DIRECTIVE")
    case object UnsupportedFeature                      extends Code("UNSUPPORTED_FEATURE")
  }
}
