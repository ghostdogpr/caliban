package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.InputValue.VariableValue
import caliban.Value.NullValue
import caliban.execution.{ ExecutionRequest, Field => F }
import caliban.introspection.Introspector
import caliban.introspection.adt._
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveDefinition
import caliban.parsing.adt.Definition.{ TypeSystemDefinition, TypeSystemExtension }
import caliban.parsing.adt.OperationType._
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt._
import caliban.rendering.DocumentRenderer
import caliban.schema._
import caliban.{ Configurator, InputValue }
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.{ Exit, IO, Trace }

import scala.annotation.tailrec
import scala.collection.compat._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Validator {
  import ValidationOps._
  import caliban.syntax._

  /**
   * A QueryValidation is a pure program that can access a Context, fail with a ValidationError or succeed with Unit.
   */
  type QueryValidation            = Context => Either[ValidationError, Unit]
  private type OptionalValidation = Option[Either[ValidationError, Unit]]

  val AllValidations: List[QueryValidation] =
    List(
      validateFragmentSpreads,
      validateOperationNameUniqueness,
      validateLoneAnonymousOperation,
      validateDirectives,
      validateVariables,
      validateSubscriptionOperation,
      validateDocumentFields
    )

  /**
   * Verifies that the given document is valid for this type.
   * Fails with a [[caliban.CalibanError.ValidationError]] otherwise.
   */
  def validate(document: Document, rootType: RootType)(implicit trace: Trace): IO[ValidationError, Unit] =
    Configurator.ref.getWith(v => Exit.fromEither(check(document, rootType, Map.empty, v.validations).unit))

  /**
   * Verifies that the given document is valid for this type for all available validations.
   * Fails with a [[caliban.CalibanError.ValidationError]] otherwise.
   */
  def validateAll(document: Document, rootType: RootType): Either[ValidationError, Unit] =
    check(document, rootType, Map.empty, AllValidations).unit

  def failValidation(msg: String, explanatoryText: String): Either[ValidationError, Nothing] =
    Left(ValidationError(msg, explanatoryText))

  /**
   * Prepare the request for execution.
   * Fails with a [[caliban.CalibanError.ValidationError]] otherwise.
   */
  def prepare(
    document: Document,
    rootType: RootType,
    operationName: Option[String],
    variables: Map[String, InputValue],
    skipValidation: Boolean,
    validations: List[QueryValidation]
  ): Either[ValidationError, ExecutionRequest] = {
    val fragments: Either[ValidationError, Map[String, FragmentDefinition]] = if (skipValidation) {
      Right(
        collectDefinitions(document)._2
          .foldLeft(List.empty[(String, FragmentDefinition)]) { case (l, f) => (f.name, f) :: l }
          .toMap
      )
    } else check(document, rootType, variables, validations)

    fragments.flatMap { fragments =>
      val operation = operationName match {
        case Some(name) =>
          document.definitions.collectFirst { case op: OperationDefinition if op.name.contains(name) => op }
            .toRight(ValidationError(s"Unknown operation $name.", ""))
        case None       =>
          document.definitions.collect { case op: OperationDefinition => op } match {
            case head :: Nil => Right(head)
            case _           => failValidation("Operation name is required.", "")
          }
      }

      operation.flatMap { op =>
        (op.operationType match {
          case Query        =>
            Right(rootType.queryType)
          case Mutation     =>
            rootType.mutationType.toRight(ValidationError("Mutations are not supported on this schema", ""))
          case Subscription =>
            rootType.subscriptionType.toRight(ValidationError("Subscriptions are not supported on this schema", ""))
        }).map(opType =>
          ExecutionRequest(
            F(
              op.selectionSet,
              fragments,
              variables,
              op.variableDefinitions,
              opType,
              document.sourceMapper,
              op.directives,
              rootType
            ),
            op.operationType,
            operationName
          )
        )
      }
    }
  }

  private def check(
    document: Document,
    rootType: RootType,
    variables: Map[String, InputValue],
    validations: List[QueryValidation]
  ): Either[ValidationError, Map[String, FragmentDefinition]] = {
    val (operations, fragments) = collectDefinitions(document)
    validateFragments(fragments).flatMap { fragmentMap =>
      val buf     = ListBuffer.empty[Selection]
      operations.foreachOne(op => collectSelectionSets(buf)(op.selectionSet))
      fragments.foreachOne(f => collectSelectionSets(buf)(f.selectionSet))
      val context = Context(document, rootType, operations, fragmentMap, buf.result(), variables)
      try
        validateAllDiscard(validations)(_.apply(context)).as(fragmentMap)
      catch {
        case _: StackOverflowError => failValidation("Max query depth exceeded", "")
      }
    }
  }

  private def collectDefinitions(document: Document): (List[OperationDefinition], List[FragmentDefinition]) =
    document.definitions.foldLeft(
      (
        List.empty[OperationDefinition],
        List.empty[FragmentDefinition]
      )
    ) {
      case ((operations, fragments), o: OperationDefinition) => (o :: operations, fragments)
      case ((operations, fragments), f: FragmentDefinition)  => (operations, f :: fragments)
      case (t, _)                                            => t
    }

  private def collectVariablesUsed(context: Context, selectionSet: List[Selection]): mutable.Set[String] = {
    val allValues = ListBuffer.empty[InputValue]
    val variables = mutable.Set.empty[String]
    val seen      = mutable.HashSet.empty[String]

    def collectValues(selectionSet: List[Selection]): Unit = {
      // ugly mutable code but it's worth it for the speed ;)
      def add(args: Map[String, InputValue]): Unit =
        if (!args.isEmpty) allValues.addAll(args.values)

      def collectDirectives(d: List[Directive]): Unit =
        d.foreachOne(d => add(d.arguments))

      selectionSet.foreachOne {
        case f: Field          =>
          add(f.arguments)
          collectDirectives(f.directives)
          val set = f.selectionSet
          if (set ne Nil) collectValues(set)
        case f: FragmentSpread =>
          val name = f.name
          if (seen.add(name)) {
            collectDirectives(f.directives)
            val f0 = context.fragments.getOrElseNull(name)
            if (f0 ne null) {
              collectDirectives(f0.directives)
              val set = f0.selectionSet
              if (set ne Nil) collectValues(set)
            }
          }
        case f: InlineFragment =>
          collectDirectives(f.dirs)
          val set = f.selectionSet
          if (set ne Nil) collectValues(set)
      }
    }

    def collectVariableValues(values: Iterable[InputValue]): Unit =
      values.foreach {
        case v: InputValue.ListValue   => collectVariableValues(v.values)
        case v: InputValue.ObjectValue => collectVariableValues(v.fields.values)
        case v: VariableValue          => variables.add(v.name)
        case _                         => ()
      }

    collectValues(selectionSet)
    if (!allValues.isEmpty) collectVariableValues(allValues)
    variables
  }

  private def collectSelectionSets(
    buffer: ListBuffer[Selection] = ListBuffer.empty
  )(selectionSet: List[Selection]): ListBuffer[Selection] = {
    def loop(selectionSet: List[Selection]): Unit =
      if (selectionSet ne Nil) {
        buffer addAll selectionSet
        selectionSet.foreachOne {
          case f: Field          => loop(f.selectionSet)
          case f: InlineFragment => loop(f.selectionSet)
          case _: FragmentSpread => ()
        }
      }
    loop(selectionSet)
    buffer
  }

  private def collectAllDirectives(
    context: Context
  ): Either[ValidationError, List[(Directive, __DirectiveLocation)]] = {
    val directiveDefinitions = context.document.directiveDefinitions.groupBy(_.name)
    val ops                  = context.operations
    for {
      _                   <- validateAllDiscard(ops)(op => checkDirectivesUniqueness(op.directives, directiveDefinitions))
      fragmentDirs         = {
        val fr = context.fragments
        if (fr.isEmpty) Nil else fr.values.toList.map(_.directives)
      }
      _                   <- validateAllDiscard(fragmentDirs)(checkDirectivesUniqueness(_, directiveDefinitions))
      selectionDirectives <- collectDirectives(context.selectionSets, directiveDefinitions)
    } yield {
      val all = ListBuffer.empty[(Directive, __DirectiveLocation)]
      ops.foreachOne { op =>
        val dirs = op.directives
        if (dirs ne Nil) {
          val location = op.operationType match {
            case OperationType.Query        => __DirectiveLocation.QUERY
            case OperationType.Mutation     => __DirectiveLocation.MUTATION
            case OperationType.Subscription => __DirectiveLocation.SUBSCRIPTION
          }
          dirs.foreachOne(v => all.addOne((v, location)))
        }
      }
      fragmentDirs.foreachOne(_.foreachOne(v => all.addOne((v, __DirectiveLocation.FRAGMENT_DEFINITION))))
      if (selectionDirectives ne Nil) all.addAll(selectionDirectives)
      if (all.isEmpty) Nil else all.result()
    }
  }

  private val RightNil = Right(Nil)

  private def collectDirectives(
    selectionSet: List[Selection],
    directiveDefinitions: Map[String, List[DirectiveDefinition]]
  ): Either[ValidationError, List[(Directive, __DirectiveLocation)]] = {
    val builder = ListBuffer.empty[List[(Directive, __DirectiveLocation)]]

    def loop(selectionSet: List[Selection]): Unit =
      selectionSet.foreachOne {
        case f: Field          =>
          val directives = f.directives
          if (directives ne Nil) builder addOne directives.map((_, __DirectiveLocation.FIELD))
          loop(f.selectionSet)
        case f: FragmentSpread =>
          val directives = f.directives
          if (directives ne Nil) builder addOne directives.map((_, __DirectiveLocation.FRAGMENT_SPREAD))
        case f: InlineFragment =>
          val directives = f.dirs
          if (directives ne Nil) builder addOne directives.map((_, __DirectiveLocation.INLINE_FRAGMENT))
          loop(f.selectionSet)
      }

    loop(selectionSet)
    if (builder.isEmpty) {
      RightNil
    } else {
      val directiveLists = builder.result()
      validateAllDiscard(directiveLists)(list => checkDirectivesUniqueness(list.map(_._1), directiveDefinitions))
        .as(directiveLists.flatten)
    }
  }

  private def checkDirectivesUniqueness(
    directives: List[Directive],
    directiveDefinitions: Map[String, List[DirectiveDefinition]]
  ): Either[ValidationError, Unit] =
    directives
      .groupBy(_.name)
      .find { case (n, v) =>
        // only non-repeatable directives count against uniqueness
        // this .lengthCompare is a 2.12 compatible version of .lengthIs and can be replaced after 2.12 support is dropped
        // it's a minor optimization to short-circuit the length check on a List for the off-chance that list is long
        (v.lengthCompare(1) > 0) && !directiveDefinitions.get(n).exists(_.exists(_.isRepeatable))
      } match {
      case None            => unit
      case Some((name, _)) =>
        failValidation(
          s"Directive '$name' is defined more than once.",
          "Directives are used to describe some metadata or behavioral change on the definition they apply to. When more than one directive of the same name is used, the expected metadata or behavior becomes ambiguous, therefore only one of each non-repeatable directive is allowed per location."
        )
    }

  def validateDirectives(context: Context): Either[ValidationError, Unit] =
    for {
      directives <- collectAllDirectives(context)
      _          <- validateAllDiscard(directives) { case (d, location) =>
                      (context.rootType.additionalDirectives ::: Introspector.directives).find(_.name == d.name) match {
                        case None            =>
                          failValidation(
                            s"Directive '${d.name}' is not supported.",
                            "GraphQL servers define what directives they support. For each usage of a directive, the directive must be available on that server."
                          )
                        case Some(directive) =>
                          validateAllDiscard(d.arguments) { (arg, argValue) =>
                            directive.allArgs.find(_.name == arg) match {
                              case Some(inputValue) =>
                                validateInputValues(
                                  inputValue,
                                  argValue,
                                  context,
                                  s"InputValue '${inputValue.name}' of Directive '${d.name}'"
                                )
                              case None             =>
                                failValidation(
                                  s"Argument '$arg' is not defined on directive '${d.name}' ($location).",
                                  "Every argument provided to a field or directive must be defined in the set of possible arguments of that field or directive."
                                )
                            }
                          } *>
                            failWhen(!directive.locations.contains(location))(
                              s"Directive '${d.name}' is used in invalid location '$location'.",
                              "GraphQL servers define what directives they support and where they support them. For each usage of a directive, the directive must be used in a location that the server has declared support for."
                            )
                      }
                    }
    } yield ()

  def validateVariables(context: Context): Either[ValidationError, Unit] =
    validateAllDiscard(context.operations) { op =>
      val variableDefinitions = op.variableDefinitions
      val variableUsages      = collectVariablesUsed(context, op.selectionSet)
      if (variableDefinitions.isEmpty && variableUsages.isEmpty) unit
      else
        validateAllDiscard(op.variableDefinitions.groupBy(_.name)) { (name, variables) =>
          failWhen(variables.sizeCompare(1) > 0)(
            s"Variable '$name' is defined more than once.",
            "If any operation defines more than one variable with the same name, it is ambiguous and invalid. It is invalid even if the type of the duplicate variable is the same."
          )
        } *> validateAllDiscard(op.variableDefinitions) { v =>
          val t = context.rootType.types.get(Type.innerType(v.variableType))

          val v1 = failWhen(t.map(_.kind).exists {
            case __TypeKind.OBJECT | __TypeKind.UNION | __TypeKind.INTERFACE => true
            case _                                                           => false
          })(
            s"Type of variable '${v.name}' is not a valid input type.",
            "Variables can only be input types. Objects, unions, and interfaces cannot be used as inputs."
          )

          val v2 =
            if (t.exists(_._isOneOfInput))
              Some(
                failWhen(v.variableType.nullable)(
                  s"Variable '${v.name}' cannot be nullable.",
                  "Variables used for OneOf Input Object fields must be non-nullable."
                ) *> validateOneOfInputValue(
                  context.variables.getOrElse(v.name, NullValue),
                  s"Variable '${v.name}'"
                )
              )
            else None

          v2.fold(v1)(v1 *> _)
        } *> {
          validateAllDiscard(variableUsages)(v =>
            failWhen(!op.variableDefinitions.exists(_.name == v))(
              s"Variable '$v' is not defined.",
              "Variables are scoped on a per‐operation basis. That means that any variable used within the context of an operation must be defined at the top level of that operation"
            )
          ) *> validateAllDiscard(op.variableDefinitions)(v =>
            failWhen(!variableUsages.contains(v.name))(
              s"Variable '${v.name}' is not used.",
              "All variables defined by an operation must be used in that operation or a fragment transitively included by that operation. Unused variables cause a validation error."
            )
          )
        }
    }

  private def collectFragmentSpreads(selectionSet: List[Selection]): List[FragmentSpread] =
    selectionSet.collect { case f: FragmentSpread => f }

  def validateFragmentSpreads(context: Context): Either[ValidationError, Unit] =
    if (context.fragments.isEmpty) unit
    else {
      val spreads     = collectFragmentSpreads(context.selectionSets)
      val spreadNames = mutable.Set.from(spreads.map(_.name))
      validateAllDiscard(context.fragments) { (name, f) =>
        if (!spreadNames.contains(name))
          failValidation(
            s"Fragment '$name' is not used in any spread.",
            "Defined fragments must be used within a document."
          )
        else if (detectCycles(context, f))
          failValidation(
            s"Fragment '$name' forms a cycle.",
            "The graph of fragment spreads must not form any cycles including spreading itself. Otherwise an operation could infinitely spread or infinitely execute on cycles in the underlying data."
          )
        else unit
      }
    }

  private def detectCycles(
    context: Context,
    fragment: FragmentDefinition,
    visited: Set[String] = Set.empty,
    checked: mutable.HashSet[String] = mutable.HashSet.empty
  ): Boolean = !checked.contains(fragment.name) && {
    val selectionSets     = collectSelectionSets()(fragment.selectionSet).result()
    val descendantSpreads = collectFragmentSpreads(selectionSets)
    val cycleDetected     = descendantSpreads.exists { s =>
      visited.contains(s.name) || {
        val f = context.fragments.getOrElseNull(s.name)
        (f ne null) && detectCycles(context, f, visited + s.name, checked)
      }
    }
    checked.add(fragment.name)
    cycleDetected
  }

  def validateDocumentFields(context: Context): Either[ValidationError, Unit] =
    validateAllDiscard(context.document.definitions) {
      case OperationDefinition(opType, _, _, _, selectionSet) =>
        opType match {
          case OperationType.Query        =>
            validateSelectionSet(context, selectionSet, context.rootType.queryType)
          case OperationType.Mutation     =>
            context.rootType.mutationType.fold[Either[ValidationError, Unit]](
              failValidation("Mutation operations are not supported on this schema.", "")
            )(validateSelectionSet(context, selectionSet, _))
          case OperationType.Subscription =>
            context.rootType.subscriptionType.fold[Either[ValidationError, Unit]](
              failValidation("Subscription operations are not supported on this schema.", "")
            )(validateSelectionSet(context, selectionSet, _))
        }
      case _: FragmentDefinition                              => unit
      case _: TypeSystemDefinition                            => unit
      case _: TypeSystemExtension                             => unit
    }

  private def containsFragments(selectionSet: List[Selection]): Boolean =
    selectionSet.exists {
      case f: Selection.Field          => containsFragments(f.selectionSet)
      case _: Selection.InlineFragment => true
      case _: Selection.FragmentSpread => true
    }

  private type ValidatedFragments = mutable.HashSet[(String, Option[String])]

  private def validateSelectionSet(
    context: Context,
    selectionSet: List[Selection],
    currentType: __Type
  ): Either[ValidationError, Unit] = {
    val v1 = validateFields(context, selectionSet, currentType)(mutable.HashSet.empty)
    if (!context.fragments.isEmpty || containsFragments(selectionSet))
      v1 *> FragmentValidator.findConflictsWithinSelectionSet(context, context.rootType.queryType, selectionSet)
    else v1
  }

  private def validateFields(
    context: Context,
    selectionSet: List[Selection],
    currentType: __Type
  )(implicit checked: ValidatedFragments): Either[ValidationError, Unit] = {
    val v1 = validateAllDiscard(selectionSet) {
      case f: Field          =>
        validateField(context, f, currentType)
      case f: FragmentSpread =>
        val name = f.name
        context.fragments.getOrElseNull(name) match {
          case null                                              =>
            failValidation(
              s"Fragment spread '$name' is not defined.",
              "Named fragment spreads must refer to fragments defined within the document. It is a validation error if the target of a spread is not defined."
            )
          case fragment if checked.add((name, currentType.name)) =>
            validateSpread(context, Some(name), currentType, Some(fragment.typeCondition), fragment.selectionSet)
          case _                                                 =>
            unit
        }
      case f: InlineFragment =>
        validateSpread(context, None, currentType, f.typeCondition, f.selectionSet)
    }
    val v2 = validateLeafFieldSelection(currentType, selectionSet ne Nil)
    v2.fold(v1)(v1 *> _)
  }

  private def validateSpread(
    context: Context,
    name: Option[String],
    currentType: __Type,
    typeCondition: Option[NamedType],
    selectionSet: List[Selection]
  )(implicit v: ValidatedFragments): Either[ValidationError, Unit] =
    typeCondition.fold(currentType)(t => context.rootType.types.getOrElseNull(t.name)) match {
      case null         =>
        val typeConditionName = typeCondition.fold("?")(_.name)
        failValidation(
          s"${name.fold("Inline fragment spread")(n => s"Fragment spread '$n'")} targets an invalid type: '$typeConditionName'.",
          "Fragments must be specified on types that exist in the schema. This applies for both named and inline fragments. If they are not defined in the schema, the query does not validate."
        )
      case fragmentType =>
        val v1 = validateFragmentType(name, fragmentType)
        val v2 = {
          val possibleTypes         = currentType.possibleTypeNames
          val possibleFragmentTypes = fragmentType.possibleTypeNames
          val hasApplicableTypes    = possibleFragmentTypes.exists(possibleTypes.contains)
          if (!hasApplicableTypes)
            failValidation(
              s"${name.fold("Inline fragment spread")(n => s"Fragment spread '$n'")} is not possible: possible types are '${possibleTypes
                  .mkString(", ")}' and possible fragment types are '${possibleFragmentTypes.mkString(", ")}'.",
              "Fragments are declared on a type and will only apply when the runtime object type matches the type condition. They also are spread within the context of a parent type. A fragment spread is only valid if its type condition could ever apply within the parent type."
            )
          else validateFields(context, selectionSet, fragmentType)
        }
        v1.fold(v2)(_ *> v2)
    }

  private def combineOptionalValidations(v1: OptionalValidation, v2: OptionalValidation): OptionalValidation =
    (v1, v2) match {
      case (None, None)         => None
      case (Some(v1), None)     => Some(v1)
      case (None, Some(v2))     => Some(v2)
      case (Some(v1), Some(v2)) => Some(v1 *> v2)
    }

  private def validateField(
    context: Context,
    field: Field,
    currentType: __Type
  )(implicit v: ValidatedFragments): Either[ValidationError, Unit] =
    if (field.name != "__typename") {
      currentType.getFieldOrNull(field.name) match {
        case null =>
          failValidation(
            s"Field '${field.name}' does not exist on type '${DocumentRenderer.renderTypeName(currentType)}'.",
            "The target field of a field selection must be defined on the scoped type of the selection set. There are no limitations on alias names."
          )
        case f    =>
          val v1 = validateFields(context, field.selectionSet, f._type.innerType)
          val v2 = validateArguments(field, f, currentType, context)
          v2.fold(v1)(v1 *> _)
      }
    } else unit

  private def validateArguments(
    field: Field,
    f: __Field,
    currentType: __Type,
    context: Context
  ): OptionalValidation = {
    val fieldArgs        = f.allArgs
    val fieldArgsNonNull = fieldArgs.filter(_._type.kind == __TypeKind.NON_NULL)
    val providedArgs     = field.arguments

    val v1 = validateAllNonEmpty(fieldArgsNonNull.flatMap { arg =>
      val arg0 = field.arguments.getOrElseNull(arg.name)
      val opt1 = (arg.defaultValue, arg0) match {
        case (None, null) | (None, NullValue) =>
          Some(
            failValidation(
              s"Required argument '${arg.name}' is null or missing on field '${field.name}' of type '${currentType.name
                  .getOrElse("")}'.",
              "Arguments can be required. An argument is required if the argument type is non‐null and does not have a default value. Otherwise, the argument is optional."
            )
          )
        case (Some(_), NullValue)             =>
          Some(
            failValidation(
              s"Required argument '${arg.name}' is null on '${field.name}' of type '${currentType.name
                  .getOrElse("")}'.",
              "Arguments can be required. An argument is required if the argument type is non‐null and does not have a default value. Otherwise, the argument is optional."
            )
          )
        case _                                => None
      }
      val opt2 =
        if (arg._type.innerType._isOneOfInput)
          Some(
            validateOneOfInputValue(
              if (arg0 eq null) NullValue else arg0,
              s"Argument '${arg.name}' on field '${field.name}'"
            )
          )
        else None

      combineOptionalValidations(opt1, opt2)
    })(identity)

    val v2 =
      if (providedArgs.isEmpty) None
      else
        Some(validateAllDiscard(providedArgs) { (arg, argValue) =>
          fieldArgs.find(_.name == arg) match {
            case Some(inputValue) =>
              validateInputValues(
                inputValue,
                argValue,
                context,
                s"InputValue '${inputValue.name}' of Field '${field.name}'"
              )
            case None             =>
              failValidation(
                s"Argument '$arg' is not defined on field '${field.name}' of type '${currentType.name.getOrElse("")}'.",
                "Every argument provided to a field or directive must be defined in the set of possible arguments of that field or directive."
              )
          }
        })

    combineOptionalValidations(v1, v2)
  }

  private[caliban] def validateInputValues(
    inputValue: __InputValue,
    argValue: InputValue,
    context: Context,
    errorContext: => String
  ): Either[ValidationError, Unit] = {
    val t           = inputValue._type
    val inputType   = if (t.kind == __TypeKind.NON_NULL) t.ofType.getOrElse(t) else t
    val inputFields = inputType.allInputFields

    argValue match {
      case InputValue.ObjectValue(fields) if inputType.kind == __TypeKind.INPUT_OBJECT =>
        validateAllDiscard(fields) { (k, v) =>
          inputFields.find(_.name == k) match {
            case None        =>
              failValidation(
                s"Input field '$k' is not defined on type '${inputType.name.getOrElse("?")}'.",
                "Every input field provided in an input object value must be defined in the set of possible fields of that input object’s expected type."
              )
            case Some(value) =>
              validateInputValues(
                value,
                v,
                context,
                s"InputValue '${inputValue.name}' of Field '$k' of InputObject '${t.name.getOrElse("")}'"
              )
          }
        } *> validateAllDiscard(inputFields)(inputField =>
          failWhen(
            inputField.defaultValue.isEmpty &&
              inputField._type.kind == __TypeKind.NON_NULL &&
              fields.getOrElse(inputField.name, NullValue) == NullValue
          )(
            s"Required field '${inputField.name}' on object '${inputType.name.getOrElse("?")}' was not provided.",
            "Input object fields may be required. Much like a field may have required arguments, an input object may have required fields. An input field is required if it has a non‐null type and does not have a default value. Otherwise, the input object field is optional."
          )
        )
      case VariableValue(variableName)                                                 =>
        context.variableDefinitions.getOrElseNull(variableName) match {
          case null               =>
            failValidation(
              s"Variable '$variableName' is not defined.",
              "Variables are scoped on a per‐operation basis. That means that any variable used within the context of an operation must be defined at the top level of that operation"
            )
          case variableDefinition => checkVariableUsageAllowed(variableDefinition, inputValue)
        }
      case _                                                                           => unit
    }
  } *> ValueValidator.validateInputTypes(inputValue, argValue, context, errorContext)

  private def validateOneOfInputValue(
    inputValue: InputValue,
    errorContext: => String
  ): Either[ValidationError, Unit] = {
    val v = inputValue match {
      case InputValue.ObjectValue(fields) if fields.size == 1 => fields.headOption.map(_._2)
      case vv: InputValue.VariableValue                       => Some(vv)
      case _                                                  => None
    }
    v match {
      case None | Some(NullValue) =>
        failValidation(
          s"$errorContext is not a valid OneOf Input Object",
          "OneOf Input Object arguments must specify exactly one non-null key"
        )
      case _                      => unit
    }
  }

  private def checkVariableUsageAllowed(
    variableDefinition: VariableDefinition,
    inputValue: __InputValue
  ): Either[ValidationError, Unit] = {
    val locationType = inputValue._type
    val variableType = variableDefinition.variableType
    if (!locationType.isNullable && !variableType.nonNull) {
      val hasNonNullVariableDefaultValue = variableDefinition.defaultValue.exists(_ != NullValue)
      val hasLocationDefaultValue        = inputValue.defaultValue.nonEmpty
      if (!hasNonNullVariableDefaultValue && !hasLocationDefaultValue)
        failValidation(
          s"Variable '${variableDefinition.name}' usage is not allowed because it is nullable and doesn't have a default value.",
          "Variable usages must be compatible with the arguments they are passed to."
        )
      else {
        val nullableLocationType = locationType.ofType.getOrElse(locationType)
        checkTypesCompatible(variableDefinition.name, variableType, nullableLocationType)
      }
    } else checkTypesCompatible(variableDefinition.name, variableType, locationType)
  }

  @tailrec
  private def checkTypesCompatible(
    variableName: String,
    variableType: Type,
    locationType: __Type
  ): Either[ValidationError, Unit] = {
    val explanation = "Variable usages must be compatible with the arguments they are passed to."
    if (!locationType.isNullable) {
      if (variableType.nullable)
        failValidation(
          s"Variable '$variableName' usage is not allowed because it is nullable but it shouldn't be.",
          explanation
        )
      else {
        val nullableLocationType = locationType.ofType.getOrElse(locationType)
        val nullableVariableType = variableType.toNullable
        checkTypesCompatible(variableName, nullableVariableType, nullableLocationType)
      }
    } else if (variableType.nonNull) {
      val nullableVariableType = variableType.toNullable
      checkTypesCompatible(variableName, nullableVariableType, locationType)
    } else if (locationType.kind == __TypeKind.LIST) {
      variableType match {
        case _: Type.NamedType        =>
          failValidation(
            s"Variable '$variableName' usage is not allowed because it is a not a list but it should be.",
            explanation
          )
        case Type.ListType(ofType, _) =>
          val itemLocationType = locationType.ofType.getOrElse(locationType)
          val itemVariableType = ofType
          checkTypesCompatible(variableName, itemVariableType, itemLocationType)
      }
    } else
      variableType match {
        case Type.ListType(_, _)     =>
          failValidation(
            s"Variable '$variableName' usage is not allowed because it is a list but it should not be.",
            explanation
          )
        case Type.NamedType(name, _) =>
          failWhen(!locationType.name.contains(name))(
            s"Variable '$variableName' usage is not allowed because its type doesn't match the schema ($name instead of ${locationType.name
                .getOrElse("")}).",
            explanation
          )
      }
  }

  private def validateLeafFieldSelection(
    currentType: __Type,
    hasSelections: Boolean
  ): OptionalValidation =
    currentType.kind match {
      case __TypeKind.SCALAR | __TypeKind.ENUM if hasSelections                          =>
        Some(
          failValidation(
            s"Field selection is impossible on type '${currentType.name.getOrElse("")}'.",
            "Field selections on scalars or enums are never allowed, because they are the leaf nodes of any GraphQL query."
          )
        )
      case __TypeKind.INTERFACE | __TypeKind.UNION | __TypeKind.OBJECT if !hasSelections =>
        Some(
          failValidation(
            s"Field selection is mandatory on type '${currentType.name.getOrElse("")}'.",
            "Leaf selections on objects, interfaces, and unions without subfields are disallowed."
          )
        )
      case _                                                                             => None
    }

  def validateOperationNameUniqueness(context: Context): Either[ValidationError, Unit] = {
    val operations    = context.operations
    val names         = operations.flatMap(_.name).groupBy(identity)
    val repeatedNames = names.collect { case (name, items) if items.length > 1 => name }
    failWhen(!repeatedNames.isEmpty)(
      s"Multiple operations have the same name: ${repeatedNames.mkString(", ")}.",
      "Each named operation definition must be unique within a document when referred to by its name."
    )
  }

  def validateLoneAnonymousOperation(context: Context): Either[ValidationError, Unit] = {
    val operations = context.operations
    val anonymous  = operations.filter(_.name.isEmpty)
    failWhen(operations.length > 1 && (anonymous ne Nil))(
      "Found both anonymous and named operations.",
      "GraphQL allows a short‐hand form for defining query operations when only that one operation exists in the document."
    )
  }

  private def validateFragments(
    fragments: List[FragmentDefinition]
  ): Either[ValidationError, Map[String, FragmentDefinition]] = {
    var fragmentMap = Map.empty[String, FragmentDefinition]
    val iter        = fragments.iterator
    while (iter.hasNext) {
      val fragment = iter.next()
      if (fragmentMap.contains(fragment.name)) {
        return Left(
          ValidationError(
            s"Fragment '${fragment.name}' is defined more than once.",
            "Fragment definitions are referenced in fragment spreads by name. To avoid ambiguity, each fragment’s name must be unique within a document."
          )
        )
      }
      fragmentMap = fragmentMap.updated(fragment.name, fragment)
    }
    Right(fragmentMap)
  }

  def validateSubscriptionOperation(context: Context): Either[ValidationError, Unit] =
    (for {
      t           <- context.rootType.subscriptionType
      op          <- context.operations.find(_.operationType == OperationType.Subscription)
      field        = F(
                       op.selectionSet,
                       context.fragments,
                       Map.empty[String, InputValue],
                       List.empty[VariableDefinition],
                       t,
                       SourceMapper.empty,
                       Nil,
                       context.rootType
                     )
      subscription = op.name.fold("")(n => s"'$n'")
      error       <- field.fields match {
                       case Nil         => None
                       case head :: Nil =>
                         if (head.name == "__typename")
                           Some(
                             ValidationError(
                               s"Subscription $subscription has a field named '__typename'.",
                               "The root field of a subscription operation must not be an introspection field."
                             )
                           )
                         else None
                       case _           =>
                         Some(
                           ValidationError(
                             s"Subscription $subscription has more than one root field.",
                             "Subscription operations must have exactly one root field."
                           )
                         )
                     }
    } yield error)
      .toLeft(())

  private def validateFragmentType(
    name: Option[String],
    targetType: __Type
  ): OptionalValidation =
    targetType.kind match {
      case __TypeKind.UNION | __TypeKind.INTERFACE | __TypeKind.OBJECT => None
      case _                                                           =>
        val targetTypeName = targetType.name.getOrElse("")
        Some(
          failValidation(
            s"${name.fold("Inline fragment")(n => s"Fragment '$n'")} is defined on invalid type '$targetTypeName'",
            "Fragments can only be declared on unions, interfaces, and objects. They are invalid on scalars. They can only be applied on non‐leaf fields. This rule applies to both inline and named fragments."
          )
        )
    }

}
