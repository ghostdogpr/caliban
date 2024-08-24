package caliban.wrappers

import caliban.{ CalibanError, Configurator, GraphQL, GraphQLAspect, InputValue, Value }
import caliban.execution.{ ExecutionRequest, Feature }
import caliban.introspection.adt.{ __Directive, __DirectiveLocation, __InputValue, __Type }
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Selection.{ Field, FragmentSpread }
import caliban.parsing.adt.{ Directive, Directives, Document, OperationType, Selection, VariableDefinition }
import caliban.schema.Types
import caliban.validation.ValidationOps.validateAllDiscard
import caliban.validation.Validator.QueryValidation
import caliban.wrappers.Wrapper.ValidationWrapper
import zio.ZIO

import scala.annotation.tailrec
import scala.collection.mutable

object IncrementalDelivery {

  private[caliban] val deferDirective = __Directive(
    Directives.Defer,
    Some(""),
    Set(__DirectiveLocation.FRAGMENT_SPREAD, __DirectiveLocation.INLINE_FRAGMENT),
    _ =>
      List(__InputValue("if", None, () => Types.boolean, None), __InputValue("label", None, () => Types.string, None)),
    isRepeatable = false
  )

  private[caliban] val streamDirective = __Directive(
    "stream",
    Some(""),
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
    var error: CalibanError.ValidationError            = null
    val checked: mutable.Set[(String, Option[String])] = mutable.Set.empty
    val iter                                           = context.operations.iterator

    def validateFields(selectionSet: List[Selection], currentType: __Type) =
      validateAllDiscard(selectionSet) {
        case f: Field                         =>
          validateField(f, currentType)
        case FragmentSpread(name, directives) =>
          context.fragments.getOrElse(name, null) match {
            case null                                              => Left(CalibanError.ValidationError(s"Fragment $name not found", ""))
            case fragment if checked.add((name, currentType.name)) =>
              validateSpread()
          }

      }

    def validateSpread()

    def validateField(field: Field, currentType: __Type): Either[CalibanError.ValidationError, Unit] = {}

//    @tailrec
//    def loop(selectionSet: List[Selection], currentType: __Type): Boolean = fields match {
//      case field :: rest =>
//        !field.fieldType.isList && field.directives.exists(_.name == Directives.Stream) || loop(field.fields ++ rest)
//      case Nil           =>
//        false
//    }
//
//    while (iter.hasNext && (error eq null)) {
//      val op    = iter.next()
//      val field = Field(
//        op.selectionSet,
//        context.fragments,
//        Map.empty[String, InputValue],
//        List.empty[VariableDefinition],
//        context.rootType.queryType,
//        SourceMapper.empty,
//        Nil,
//        context.rootType
//      )
//
//      if (loop(List(field))) {
//        error = CalibanError.ValidationError("Stream directive was used on a non-list field", "")
//      }
//    }

    if (error ne null) Left(error) else Right(())
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

  private def withValidations(features: Set[Feature]): GraphQLAspect[Nothing, Any] = new ValidationWrapper[Any] {
    private val validations = additionalValidations(features)

    override def wrap[R1 <: Any](
      f: Document => ZIO[R1, CalibanError.ValidationError, ExecutionRequest]
    ): Document => ZIO[R1, CalibanError.ValidationError, ExecutionRequest] = { doc =>
      Configurator.ref
        .locallyWith(config => config.copy(validations = config.validations ++ validations))(f(doc))
    }
  }

  def aspect(feature: Feature, others: Feature*): GraphQLAspect[Nothing, Any] = new GraphQLAspect[Nothing, Any] {
    private val featureSet = Set(feature) ++ others

    override def apply[R](gql: GraphQL[R]): GraphQL[R] =
      (gql @@ withValidations(featureSet))
        .enableAll(featureSet)
        .withAdditionalDirectives(
          List(
            Some(deferDirective).filter(_ => featureSet(Feature.Defer)),
            Some(streamDirective).filter(_ => featureSet(Feature.Stream))
          ).flatten
        )
  }

}
