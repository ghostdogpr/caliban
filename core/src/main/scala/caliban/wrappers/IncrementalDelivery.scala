package caliban.wrappers

import caliban.execution.{ ExecutionRequest, Feature }
import caliban.introspection.adt.{ __Directive, __DirectiveLocation, __InputValue, __Type }
import caliban.parsing.adt.Definition.ExecutableDefinition.FragmentDefinition
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt._
import caliban.schema.Types
import caliban.validation.ValidationOps.validateAllDiscard
import caliban.validation.Validator.{ failValidation, QueryValidation }
import caliban.wrappers.Wrapper.ValidationWrapper
import caliban._
import zio.ZIO

import scala.annotation.tailrec
import scala.collection.mutable

object IncrementalDelivery {

  private[caliban] val deferDirective = __Directive(
    Directives.Defer,
    Some(
      "Marks a fragment as being optionally deferrable. Allowing the backend to split the query and return non-deferred parts first. This implicitly uses a streaming transport protocol which requires client support."
    ),
    Set(__DirectiveLocation.FRAGMENT_SPREAD, __DirectiveLocation.INLINE_FRAGMENT),
    _ =>
      List(__InputValue("if", None, () => Types.boolean, None), __InputValue("label", None, () => Types.string, None)),
    isRepeatable = false
  )

  private[caliban] val streamDirective = __Directive(
    Directives.Stream,
    Some(
      "Marks a field as being optionally streamable. Allowing the backend to split the returned list value into separate payloads, only returning the first part of the list immediately. This implicitly uses a streaming transport protocol which requires client support."
    ),
    Set(__DirectiveLocation.FIELD),
    _ =>
      List(
        __InputValue("if", None, () => Types.boolean, None),
        __InputValue("initialCount", None, () => Types.int, None),
        __InputValue("label", None, () => Types.string, None)
      ),
    isRepeatable = false
  )

  private val onlyTopLevelQuery: QueryValidation = context => {
    @tailrec
    def hasStreamOrDirective(selection: List[Selection]): Boolean =
      selection match {
        case Selection.Field(_, _, _, directives, _, _) :: rest   =>
          directives.exists(d => d.name == Directives.Stream || d.name == Directives.Defer) || hasStreamOrDirective(
            rest
          )
        case Selection.InlineFragment(_, _, selectionSet) :: rest =>
          hasStreamOrDirective(rest ++ selectionSet)
        case Selection.FragmentSpread(name, _) :: rest            =>
          hasStreamOrDirective(rest ++ context.fragments(name).selectionSet)
        case Nil                                                  => false
      }

    validateAllDiscard(context.operations) { op =>
      if (op.operationType != OperationType.Query && hasStreamOrDirective(op.selectionSet)) {
        Left(
          CalibanError.ValidationError(
            "Stream or defer directive was used on a root field in a mutation or subscription",
            "Defer and stream may not be used on root fields of mutations or subscriptions"
          )
        )
      } else {
        Right(())
      }
    }
  }

  private val appearsOnlyOnLists: QueryValidation = context => {
    val checked: mutable.Set[(String, Option[String])] = mutable.Set.empty

    def validateFields(selectionSet: List[Selection], currentType: __Type): Either[CalibanError.ValidationError, Unit] =
      validateAllDiscard(selectionSet) {
        case f: Field                                          =>
          validateField(f, currentType)
        case FragmentSpread(name, directives)                  =>
          if (directives.exists(_.name == Directives.Stream))
            Left(CalibanError.ValidationError("Stream directive was used on a fragment spread", ""))
          else {
            context.fragments.getOrElse(name, null) match {
              case null                                              => Left(CalibanError.ValidationError(s"Fragment $name not found", ""))
              case fragment if checked.add((name, currentType.name)) =>
                validateSpread(fragment, currentType)
              case _                                                 => Right(())
            }
          }
        case InlineFragment(typeCondition, dirs, selectionSet) =>
          if (dirs.exists(_.name == Directives.Stream))
            Left(CalibanError.ValidationError("Stream directive was used on an inline fragment", ""))
          else {
            if (typeCondition.exists(_.name.contains(currentType.typeNameRepr)))
              validateFields(selectionSet, currentType)
            else Right(())
          }

      }

    def validateSpread(fragment: FragmentDefinition, currentType: __Type): Either[CalibanError.ValidationError, Unit] =
      validateFields(fragment.selectionSet, currentType)

    def validateField(field: Field, currentType: __Type): Either[CalibanError.ValidationError, Unit] = {
      val selected = currentType.allFields.find(_.name == field.name)
      Either.cond(
        selected.isDefined && !selected.get._type.isList && field.directives.forall(_.name != Directives.Stream),
        (),
        CalibanError.ValidationError("Stream directive was used on a non-list field", "")
      )
    }

    validateAllDiscard(context.operations) { op =>
      op.operationType match {
        case OperationType.Query        =>
          validateFields(op.selectionSet, context.rootType.queryType)
        case OperationType.Mutation     =>
          context.rootType.mutationType.fold[Either[CalibanError.ValidationError, Unit]](
            failValidation("Mutation operations are not supported on this schema.", "")
          )(validateFields(op.selectionSet, _))
        case OperationType.Subscription =>
          context.rootType.subscriptionType.fold[Either[CalibanError.ValidationError, Unit]](
            failValidation("Subscription operations are not supported on this schema.", "")
          )(validateFields(op.selectionSet, _))
      }
    }
  }

  private val uniqueLabels: QueryValidation = context => {
    val labels = mutable.Set[String]()

    def extractLabel(directives: List[Directive], directiveName: String): Option[InputValue] =
      directives.collectFirst {
        case d if d.name == directiveName =>
          d.arguments.get("label")
      }.flatten

    def isLabelUnique(label: Option[InputValue]): Boolean = label.forall {
      case Value.StringValue(value) => labels.add(value)
      case _                        => true
    }

    @tailrec
    def allLabelsUnique(selections: List[Selection]): Boolean = selections match {
      case Selection.Field(_, _, _, directives, children, _) :: rest     =>
        val label = extractLabel(directives, Directives.Stream)

        isLabelUnique(label) && allLabelsUnique(rest ++ children)
      case Selection.InlineFragment(_, directives, selectionSet) :: rest =>
        val label = extractLabel(directives, Directives.Defer)

        isLabelUnique(label) && allLabelsUnique(rest ++ selectionSet)
      case Selection.FragmentSpread(name, directives) :: rest            =>
        val label = extractLabel(directives, Directives.Defer)

        isLabelUnique(label) && allLabelsUnique(rest ++ context.fragments(name).selectionSet)
      case Nil                                                           => true
    }

    if (!allLabelsUnique(context.operations.flatMap(_.selectionSet))) {
      Left(CalibanError.ValidationError("Stream and defer directive labels must be unique", ""))
    } else {
      Right(())
    }
  }

  private def additionalValidations(features: Set[Feature]) = {
    val streamValidations =
      if (features(Feature.Stream)) List(appearsOnlyOnLists) else Nil

    List(onlyTopLevelQuery, uniqueLabels) ++ streamValidations
  }

  private def withValidations(features: Set[Feature]): Wrapper[Any] = new ValidationWrapper[Any] {
    private val validations = additionalValidations(features)

    override def wrap[R1 <: Any](
      f: Document => ZIO[R1, CalibanError.ValidationError, ExecutionRequest]
    ): Document => ZIO[R1, CalibanError.ValidationError, ExecutionRequest] = { doc =>
      Configurator.ref
        .locallyWith(config => config.copy(validations = config.validations ++ validations))(f(doc))
    }
  }

  def aspect(feature: Feature, others: Feature*): GraphQLAspect[Nothing, Any] = new GraphQLAspect[Nothing, Any] {
    private val featureSet    = Set(feature) ++ others
    private val directiveList = List(
      Some(deferDirective).filter(_ => featureSet(Feature.Defer)),
      Some(streamDirective).filter(_ => featureSet(Feature.Stream))
    ).flatten

    override def apply[R](gql: GraphQL[R]): GraphQL[R] =
      gql
        .enableAll(featureSet)
        .withAdditionalDirectives(directiveList)
        .withWrapper(withValidations(featureSet))
  }

}
