package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.InputValue.VariableValue
import caliban.Value.NullValue
import caliban.execution.{ ExecutionRequest, Field => F }
import caliban.introspection.Introspector
import caliban.introspection.adt._
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Definition.TypeSystemDefinition
import caliban.parsing.adt.OperationType._
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt._
import caliban.schema.{ RootSchema, RootType, Types }
import caliban.{ InputValue, Rendering }
import zio.IO

object Validator {

  /**
   * Verifies that the given document is valid for this type. Fails with a [[caliban.CalibanError.ValidationError]] otherwise.
   */
  def validate(document: Document, rootType: RootType): IO[ValidationError, Unit] =
    check(document, rootType).unit

  /**
   * Verifies that the given schema is valid. Fails with a [[caliban.CalibanError.ValidationError]] otherwise.
   */
  def validateSchema(rootType: RootType): IO[ValidationError, Unit] =
    IO.foreach(rootType.types.values) { t =>
        t.kind match {
          case __TypeKind.ENUM         => validateEnum(t)
          case __TypeKind.UNION        => validateUnion(t)
          case __TypeKind.INPUT_OBJECT => validateInputObject(t)
          case _                       => IO.unit
        }
      }
      .unit

  def failValidation[T](msg: String, explanatoryText: String): IO[ValidationError, T] =
    IO.fail(ValidationError(msg, explanatoryText))

  /**
   * Prepare the request for execution.
   * Fails with a [[caliban.CalibanError.ValidationError]] otherwise.
   */
  def prepare[R](
    document: Document,
    rootType: RootType,
    rootSchema: RootSchema[R],
    operationName: Option[String],
    variables: Map[String, InputValue],
    skipValidation: Boolean
  ): IO[ValidationError, ExecutionRequest] = {
    val fragments = if (skipValidation) {
      IO.succeed(collectDefinitions(document)._2.foldLeft(Map.empty[String, FragmentDefinition]) {
        case (m, f) => m.updated(f.name, f)
      })
    } else check(document, rootType)

    fragments.flatMap { fragments =>
      val operation = operationName match {
        case Some(name) =>
          document.definitions.collectFirst { case op: OperationDefinition if op.name.contains(name) => op }
            .toRight(s"Unknown operation $name.")
        case None =>
          document.definitions.collect { case op: OperationDefinition => op } match {
            case head :: Nil => Right(head)
            case _           => Left("Operation name is required.")
          }
      }

      operation match {
        case Left(error) => failValidation(error, "")
        case Right(op) =>
          (op.operationType match {
            case Query => IO.succeed(rootSchema.query)
            case Mutation =>
              rootSchema.mutation match {
                case Some(m) => IO.succeed(m)
                case None    => failValidation("Mutations are not supported on this schema", "")
              }
            case Subscription =>
              rootSchema.subscription match {
                case Some(m) => IO.succeed(m)
                case None    => failValidation("Subscriptions are not supported on this schema", "")
              }
          }).map(operation =>
            ExecutionRequest(
              F(op.selectionSet, fragments, variables, operation.opType, document.sourceMapper, Nil),
              op.operationType,
              op.variableDefinitions
            )
          )
      }
    }
  }

  private def check(document: Document, rootType: RootType): IO[ValidationError, Map[String, FragmentDefinition]] = {
    val (operations, fragments, _) = collectDefinitions(document)
    for {
      fragmentMap   <- validateFragments(fragments)
      selectionSets = collectSelectionSets(operations.flatMap(_.selectionSet) ++ fragments.flatMap(_.selectionSet))
      context       = Context(document, rootType, operations, fragmentMap, selectionSets)
      _             <- validateOperationNameUniqueness(operations)
      _             <- validateLoneAnonymousOperation(operations)
      _             <- validateDirectives(context)
      _             <- validateFragmentSpreads(context)
      _             <- validateVariables(context)
      _             <- validateSubscriptionOperation(context)
      _             <- validateDocumentFields(context)
    } yield fragmentMap
  }

  private def collectDefinitions(
    document: Document
  ): (List[OperationDefinition], List[FragmentDefinition], List[TypeSystemDefinition]) =
    document.definitions.foldLeft(
      (List.empty[OperationDefinition], List.empty[FragmentDefinition], List.empty[TypeSystemDefinition])
    ) {
      case ((operations, fragments, types), o: OperationDefinition)  => (o :: operations, fragments, types)
      case ((operations, fragments, types), f: FragmentDefinition)   => (operations, f :: fragments, types)
      case ((operations, fragments, types), t: TypeSystemDefinition) => (operations, fragments, t :: types)
    }

  private def collectVariablesUsed(context: Context, selectionSet: List[Selection]): Set[String] = {
    def collectValues(selectionSet: List[Selection]): List[InputValue] =
      selectionSet.flatMap {
        case FragmentSpread(name, directives) =>
          directives.flatMap(_.arguments.values) ++ context.fragments
            .get(name)
            .fold(List.empty[InputValue])(f =>
              f.directives.flatMap(_.arguments.values) ++ collectValues(f.selectionSet)
            )
        case Field(_, _, arguments, directives, selectionSet, _) =>
          arguments.values ++ directives.flatMap(_.arguments.values) ++ collectValues(selectionSet)
        case InlineFragment(_, directives, selectionSet) =>
          directives.flatMap(_.arguments.values) ++ collectValues(selectionSet)
      }
    collectValues(selectionSet).collect {
      case VariableValue(name) => name
    }.toSet
  }

  private def collectSelectionSets(selectionSet: List[Selection]): List[Selection] =
    selectionSet ++ selectionSet.flatMap {
      case _: FragmentSpread => Nil
      case f: Field          => collectSelectionSets(f.selectionSet)
      case f: InlineFragment => collectSelectionSets(f.selectionSet)
    }

  private def collectAllDirectives(context: Context): IO[ValidationError, List[(Directive, __DirectiveLocation)]] =
    for {
      opDirectives <- IO.foreach(context.operations)(op =>
                       checkDirectivesUniqueness(op.directives).as(op.operationType match {
                         case OperationType.Query    => op.directives.map((_, __DirectiveLocation.QUERY))
                         case OperationType.Mutation => op.directives.map((_, __DirectiveLocation.MUTATION))
                         case OperationType.Subscription =>
                           op.directives.map((_, __DirectiveLocation.SUBSCRIPTION))
                       })
                     )
      fragmentDirectives <- IO.foreach(context.fragments.values)(fragment =>
                             checkDirectivesUniqueness(fragment.directives)
                               .as(fragment.directives.map((_, __DirectiveLocation.FRAGMENT_DEFINITION)))
                           )
      selectionDirectives <- collectDirectives(context.selectionSets)
    } yield opDirectives.flatten ++ fragmentDirectives.flatten ++ selectionDirectives

  private def collectDirectives(
    selectionSet: List[Selection]
  ): IO[ValidationError, List[(Directive, __DirectiveLocation)]] =
    IO.foreach(selectionSet) {
        case FragmentSpread(_, directives) =>
          checkDirectivesUniqueness(directives).as(directives.map((_, __DirectiveLocation.FRAGMENT_SPREAD)))
        case Field(_, _, _, directives, selectionSet, _) =>
          checkDirectivesUniqueness(directives) *>
            collectDirectives(selectionSet).map(directives.map((_, __DirectiveLocation.FIELD)) ++ _)
        case InlineFragment(_, directives, selectionSet) =>
          checkDirectivesUniqueness(directives) *>
            collectDirectives(selectionSet).map(directives.map((_, __DirectiveLocation.INLINE_FRAGMENT)) ++ _)
      }
      .map(_.flatten)

  private def checkDirectivesUniqueness(directives: List[Directive]): IO[ValidationError, Unit] =
    IO.whenCase(directives.groupBy(_.name).find { case (_, v) => v.length > 1 }) {
      case Some((name, _)) =>
        failValidation(
          s"Directive '$name' is defined twice.",
          "Directives are used to describe some metadata or behavioral change on the definition they apply to. When more than one directive of the same name is used, the expected metadata or behavior becomes ambiguous, therefore only one of each directive is allowed per location."
        )
    }

  private def validateDirectives(context: Context): IO[ValidationError, Unit] =
    for {
      directives <- collectAllDirectives(context)
      _ <- IO.foreach(directives) {
            case (d, location) =>
              Introspector.directives.find(_.name == d.name) match {
                case None =>
                  failValidation(
                    s"Directive '${d.name}' is not supported.",
                    "GraphQL servers define what directives they support. For each usage of a directive, the directive must be available on that server."
                  )
                case Some(directive) =>
                  IO.when(!directive.locations.contains(location))(
                    failValidation(
                      s"Directive '${d.name}' is used in invalid location '$location'.",
                      "GraphQL servers define what directives they support and where they support them. For each usage of a directive, the directive must be used in a location that the server has declared support for."
                    )
                  )
              }
          }
    } yield ()

  private def validateVariables(context: Context): IO[ValidationError, Unit] =
    IO.foreach(context.operations)(op =>
        IO.foreach(op.variableDefinitions.groupBy(_.name)) {
          case (name, variables) =>
            IO.when(variables.length > 1)(
              failValidation(
                s"Variable '$name' is defined more than once.",
                "If any operation defines more than one variable with the same name, it is ambiguous and invalid. It is invalid even if the type of the duplicate variable is the same."
              )
            )
        } *> IO.foreach(op.variableDefinitions) { v =>
          val t = Type.innerType(v.variableType)
          IO.whenCase(context.rootType.types.get(t).map(_.kind)) {
            case Some(__TypeKind.OBJECT) | Some(__TypeKind.UNION) | Some(__TypeKind.INTERFACE) =>
              failValidation(
                s"Type of variable '${v.name}' is not a valid input type.",
                "Variables can only be input types. Objects, unions, and interfaces cannot be used as inputs."
              )
          }
        } *> {
          val variableUsages = collectVariablesUsed(context, op.selectionSet)
          IO.foreach(variableUsages)(v =>
            IO.when(!op.variableDefinitions.exists(_.name == v))(
              failValidation(
                s"Variable '$v' is not defined.",
                "Variables are scoped on a per‐operation basis. That means that any variable used within the context of an operation must be defined at the top level of that operation"
              )
            )
          ) *> IO.foreach(op.variableDefinitions)(v =>
            IO.when(!variableUsages.contains(v.name))(
              failValidation(
                s"Variable '${v.name}' is not used.",
                "All variables defined by an operation must be used in that operation or a fragment transitively included by that operation. Unused variables cause a validation error."
              )
            )
          )
        }
      )
      .unit

  private def collectFragmentSpreads(selectionSet: List[Selection]): List[FragmentSpread] =
    selectionSet.collect { case f: FragmentSpread => f }

  private def validateFragmentSpreads(context: Context): IO[ValidationError, Unit] = {
    val spreads     = collectFragmentSpreads(context.selectionSets)
    val spreadNames = spreads.map(_.name).toSet
    IO.foreach(context.fragments.values)(f =>
        if (!spreadNames.contains(f.name))
          failValidation(
            s"Fragment '${f.name}' is not used in any spread.",
            "Defined fragments must be used within a document."
          )
        else
          failValidation(
            s"Fragment '${f.name}' forms a cycle.",
            "The graph of fragment spreads must not form any cycles including spreading itself. Otherwise an operation could infinitely spread or infinitely execute on cycles in the underlying data."
          ).when(detectCycles(context, f))
      )
      .unit
  }

  private def detectCycles(context: Context, fragment: FragmentDefinition, visited: Set[String] = Set()): Boolean = {
    val selectionSets     = collectSelectionSets(fragment.selectionSet)
    val descendantSpreads = collectFragmentSpreads(selectionSets)
    descendantSpreads.exists(s =>
      visited.contains(s.name) ||
        context.fragments.get(s.name).fold(false)(f => detectCycles(context, f, visited + s.name))
    )
  }

  private def validateDocumentFields(context: Context): IO[ValidationError, Unit] =
    IO.foreach(context.document.definitions) {
        case OperationDefinition(opType, _, _, _, selectionSet) =>
          opType match {
            case OperationType.Query => validateFields(context, selectionSet, context.rootType.queryType)
            case OperationType.Mutation =>
              context.rootType.mutationType.fold[IO[ValidationError, Unit]](
                failValidation("Mutation operations are not supported on this schema.", "")
              )(validateFields(context, selectionSet, _))
            case OperationType.Subscription =>
              context.rootType.subscriptionType.fold[IO[ValidationError, Unit]](
                failValidation("Subscription operations are not supported on this schema.", "")
              )(validateFields(context, selectionSet, _))
          }
        case _: FragmentDefinition   => IO.unit
        case _: TypeSystemDefinition => IO.unit
      }
      .unit

  private def validateFields(
    context: Context,
    selectionSet: List[Selection],
    currentType: __Type
  ): IO[ValidationError, Unit] =
    IO.foreach(selectionSet) {
      case f: Field => validateField(context, f, currentType)
      case FragmentSpread(name, _) =>
        context.fragments.get(name) match {
          case None =>
            failValidation(
              s"Fragment spread '$name' is not defined.",
              "Named fragment spreads must refer to fragments defined within the document. It is a validation error if the target of a spread is not defined."
            )
          case Some(fragment) =>
            validateSpread(context, Some(name), currentType, Some(fragment.typeCondition), fragment.selectionSet)
        }
      case InlineFragment(typeCondition, _, selectionSet) =>
        validateSpread(context, None, currentType, typeCondition, selectionSet)
    } *> validateLeafFieldSelection(selectionSet, currentType)

  private def validateSpread(
    context: Context,
    name: Option[String],
    currentType: __Type,
    typeCondition: Option[NamedType],
    selectionSet: List[Selection]
  ): IO[ValidationError, Unit] =
    typeCondition.fold[Option[__Type]](Some(currentType))(t => context.rootType.types.get(t.name)) match {
      case Some(fragmentType) =>
        validateFragmentType(name, fragmentType) *> {
          val possibleTypes         = getPossibleTypes(currentType).flatMap(_.name)
          val possibleFragmentTypes = getPossibleTypes(fragmentType).flatMap(_.name)
          val applicableTypes       = possibleTypes intersect possibleFragmentTypes
          IO.when(applicableTypes.isEmpty)(
            failValidation(
              s"${name.fold("Inline fragment spread")(n => s"Fragment spread '$n'")} is not possible: possible types are '${possibleTypes
                .mkString(", ")}' and possible fragment types are '${possibleFragmentTypes.mkString(", ")}'.",
              "Fragments are declared on a type and will only apply when the runtime object type matches the type condition. They also are spread within the context of a parent type. A fragment spread is only valid if its type condition could ever apply within the parent type."
            )
          ) *> validateFields(context, selectionSet, fragmentType)
        }
      case None =>
        lazy val typeConditionName = typeCondition.fold("?")(_.name)
        failValidation(
          s"${name.fold("Inline fragment spread")(n => s"Fragment spread '$n'")} targets an invalid type: '$typeConditionName'.",
          "Fragments must be specified on types that exist in the schema. This applies for both named and inline fragments. If they are not defined in the schema, the query does not validate."
        )
    }

  private def getPossibleTypes(t: __Type): List[__Type] =
    t.kind match {
      case __TypeKind.OBJECT                       => List(t)
      case __TypeKind.INTERFACE | __TypeKind.UNION => t.possibleTypes.getOrElse(Nil)
      case _                                       => Nil
    }

  private def validateField(context: Context, field: Field, currentType: __Type): IO[ValidationError, Unit] =
    IO.when(field.name != "__typename") {
      IO.fromOption(currentType.fields(__DeprecatedArgs(Some(true))).getOrElse(Nil).find(_.name == field.name))
        .asError(
          ValidationError(
            s"Field '${field.name}' does not exist on type '${Rendering.renderTypeName(currentType)}'.",
            "The target field of a field selection must be defined on the scoped type of the selection set. There are no limitations on alias names."
          )
        )
        .flatMap { f =>
          validateFields(context, field.selectionSet, Types.innerType(f.`type`())) *>
            validateArguments(field, f, currentType)
        }
    }

  private def validateArguments(field: Field, f: __Field, currentType: __Type): IO[ValidationError, List[Unit]] =
    IO.foreach(field.arguments) {
      case (arg, argValue) =>
        f.args.find(_.name == arg) match {
          case None =>
            failValidation(
              s"Argument '$arg' is not defined on field '${field.name}' of type '${currentType.name.getOrElse("")}'.",
              "Every argument provided to a field or directive must be defined in the set of possible arguments of that field or directive."
            )
          case Some(inputValue) => validateInputValues(inputValue, argValue)
        }
    } *>
      IO.foreach(f.args.filter(a => a.`type`().kind == __TypeKind.NON_NULL && a.defaultValue.isEmpty))(arg =>
        IO.when(field.arguments.get(arg.name).forall(_ == NullValue))(
          failValidation(
            s"Required argument '${arg.name}' is null or missing on field '${field.name}' of type '${currentType.name
              .getOrElse("")}'.",
            "Arguments can be required. An argument is required if the argument type is non‐null and does not have a default value. Otherwise, the argument is optional."
          )
        )
      )

  private def validateInputValues(inputValue: __InputValue, argValue: InputValue): IO[ValidationError, Unit] = {
    val t           = inputValue.`type`()
    val inputType   = if (t.kind == __TypeKind.NON_NULL) t.ofType.getOrElse(t) else t
    val inputFields = inputType.inputFields.getOrElse(Nil)
    argValue match {
      case InputValue.ObjectValue(fields) if inputType.kind == __TypeKind.INPUT_OBJECT =>
        IO.foreach(fields) {
          case (k, v) =>
            inputFields.find(_.name == k) match {
              case None =>
                failValidation(
                  s"Input field '$k' is not defined on type '${inputType.name.getOrElse("?")}'.",
                  "Every input field provided in an input object value must be defined in the set of possible fields of that input object’s expected type."
                )
              case Some(value) => validateInputValues(value, v)
            }
        } *> IO
          .foreach(inputFields)(inputField =>
            IO.when(
              inputField.defaultValue.isEmpty &&
                inputField.`type`().kind == __TypeKind.NON_NULL &&
                !fields.contains(inputField.name)
            )(
              failValidation(
                s"Required field '${inputField.name}' on object '${inputType.name.getOrElse("?")}' was not provided.",
                "Input object fields may be required. Much like a field may have required arguments, an input object may have required fields. An input field is required if it has a non‐null type and does not have a default value. Otherwise, the input object field is optional."
              )
            )
          )
          .unit
      case _ => IO.unit
    }
  }

  private def validateLeafFieldSelection(selections: List[Selection], currentType: __Type): IO[ValidationError, Unit] =
    IO.whenCase(currentType.kind) {
      case __TypeKind.SCALAR | __TypeKind.ENUM if selections.nonEmpty =>
        failValidation(
          s"Field selection is impossible on type '${currentType.name.getOrElse("")}'.",
          "Field selections on scalars or enums are never allowed, because they are the leaf nodes of any GraphQL query."
        )
      case __TypeKind.INTERFACE | __TypeKind.UNION | __TypeKind.OBJECT if selections.isEmpty =>
        failValidation(
          s"Field selection is mandatory on type '${currentType.name.getOrElse("")}'.",
          "Leaf selections on objects, interfaces, and unions without subfields are disallowed."
        )
    }

  private def validateOperationNameUniqueness(operations: List[OperationDefinition]): IO[ValidationError, Unit] = {
    val names         = operations.flatMap(_.name).groupBy(identity)
    val repeatedNames = names.collect { case (name, items) if items.length > 1 => name }
    IO.when(repeatedNames.nonEmpty)(
      failValidation(
        s"Multiple operations have the same name: ${repeatedNames.mkString(", ")}.",
        "Each named operation definition must be unique within a document when referred to by its name."
      )
    )
  }

  private def validateLoneAnonymousOperation(operations: List[OperationDefinition]): IO[ValidationError, Unit] = {
    val anonymous = operations.filter(_.name.isEmpty)
    IO.when(operations.length > 1 && anonymous.nonEmpty)(
      failValidation(
        "Found both anonymous and named operations.",
        "GraphQL allows a short‐hand form for defining query operations when only that one operation exists in the document."
      )
    )
  }

  private def validateFragments(
    fragments: List[FragmentDefinition]
  ): IO[ValidationError, Map[String, FragmentDefinition]] =
    IO.foldLeft(fragments)(Map.empty[String, FragmentDefinition]) {
      case (fragmentMap, fragment) =>
        if (fragmentMap.contains(fragment.name)) {
          failValidation(
            s"Fragment '${fragment.name}' is defined more than once.",
            "Fragment definitions are referenced in fragment spreads by name. To avoid ambiguity, each fragment’s name must be unique within a document."
          )
        } else IO.succeed(fragmentMap.updated(fragment.name, fragment))
    }

  private def validateSubscriptionOperation(context: Context): IO[ValidationError, Unit] =
    IO.fromOption(
        for {
          t <- context.rootType.subscriptionType
          op <- context.operations
                 .filter(_.operationType == OperationType.Subscription)
                 .find(op =>
                   F(op.selectionSet, context.fragments, Map.empty[String, InputValue], t, SourceMapper.empty, Nil).fields.length > 1
                 )
        } yield op
      )
      .map { op =>
        val subscription = op.name.fold("")(n => s"'$n'")
        ValidationError(
          s"Subscription $subscription has more than one root field.",
          "Subscription operations must have exactly one root field."
        )
      }
      .flip

  private def validateFragmentType(name: Option[String], targetType: __Type): IO[ValidationError, Unit] =
    targetType.kind match {
      case __TypeKind.UNION | __TypeKind.INTERFACE | __TypeKind.OBJECT => IO.unit
      case _ =>
        val targetTypeName = targetType.name.getOrElse("")
        failValidation(
          s"${name.fold("Inline fragment")(n => s"Fragment '$n'")} is defined on invalid type '$targetTypeName'",
          "Fragments can only be declared on unions, interfaces, and objects. They are invalid on scalars. They can only be applied on non‐leaf fields. This rule applies to both inline and named fragments."
        )
    }

  private def validateEnum(t: __Type): IO[ValidationError, Unit] =
    t.enumValues(__DeprecatedArgs(Some(true))) match {
      case Some(_ :: _) => IO.unit
      case _ =>
        failValidation(
          s"Enum ${t.name.getOrElse("")} doesn't contain any values",
          "An Enum type must define one or more unique enum values."
        )
    }

  private def validateUnion(t: __Type): IO[ValidationError, Unit] = {

    def isObject(t: __Type): Boolean = t.kind match {
      case __TypeKind.OBJECT => true
      case _                 => false
    }

    t.possibleTypes match {
      case None | Some(Nil) =>
        failValidation(
          s"Union ${t.name.getOrElse("")} doesn't contain any type.",
          "A Union type must include one or more unique member types."
        )
      case Some(types) if !types.forall(isObject) =>
        failValidation(
          s"Union ${t.name.getOrElse("")} contains the following non Object types: " +
            types.filterNot(isObject).map(_.name.getOrElse("")).filterNot(_.isEmpty).mkString("", ", ", "."),
          s"The member types of a Union type must all be Object base types."
        )
      case _ => IO.unit
    }

  }

  private def validateInputObject(t: __Type): IO[ValidationError, Unit] = {
    // https://spec.graphql.org/June2018/#IsInputType()
    def isInputType(t: __Type): Either[__Type, Unit] = t.kind match {
      case __TypeKind.LIST | __TypeKind.NON_NULL                         => t.ofType.fold[Either[__Type, Unit]](Left(t))(isInputType)
      case __TypeKind.SCALAR | __TypeKind.ENUM | __TypeKind.INPUT_OBJECT => Right(())
      case _                                                             => Left(t)
    }

    def validateFields(fields: List[__InputValue]): IO[ValidationError, Unit] =
      duplicateFieldName(fields).flatMap(_ =>
        IO.foreach(fields)(field =>
            for {
              _ <- doesNotStartWithUnderscore(field)
              _ <- onlyInputFieldType(field)
            } yield ()
          )
          .unit
      )

    def duplicateFieldName(fields: List[__InputValue]): IO[ValidationError, Unit] =
      fields
        .groupBy(_.name)
        .collectFirst { case (_, f :: _ :: _) => f }
        .fold[IO[ValidationError, Unit]](IO.unit)(duplicateField =>
          failValidation(
            s"InputObject has repeated fields: ${duplicateField.name}",
            "The input field must have a unique name within that Input Object type; no two input fields may share the same name"
          )
        )

    def doesNotStartWithUnderscore(field: __InputValue): IO[ValidationError, Unit] =
      IO.when(field.name.startsWith("__"))(
        failValidation(
          s"InputObject can't start with '__': ${field.name}",
          """The input field must not have a name which begins with the
characters {"__"} (two underscores)"""
        )
      )

    def onlyInputFieldType(field: __InputValue): IO[ValidationError, Unit] =
      IO.whenCase(isInputType(field.`type`())) {
        case Left(errorType) =>
          failValidation(
            s"${errorType.name.getOrElse("")} is of kind ${errorType.kind}, must be an InputType",
            """The input field must accept a type where IsInputType(inputFieldType) returns true, https://spec.graphql.org/June2018/#IsInputType()"""
          )
      }

    t.inputFields match {
      case None | Some(Nil) =>
        failValidation(
          s"InputObject ${t.name.getOrElse("")} does not have fields",
          "An Input Object type must define one or more input fields"
        )
      case Some(fields) => validateFields(fields)
    }
  }

  case class Context(
    document: Document,
    rootType: RootType,
    operations: List[OperationDefinition],
    fragments: Map[String, FragmentDefinition],
    selectionSets: List[Selection]
  )

}
