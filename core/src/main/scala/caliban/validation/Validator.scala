package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.InputValue.VariableValue
import caliban.Value.NullValue
import caliban.execution.{ ExecutionRequest, Field => F }
import caliban.introspection.Introspector
import caliban.introspection.adt._
import caliban.introspection.adt.__TypeKind._
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveDefinition
import caliban.parsing.adt.Definition.{ TypeSystemDefinition, TypeSystemExtension }
import caliban.parsing.adt.OperationType._
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt._
import caliban.parsing.{ Parser, SourceMapper }
import caliban.rendering.DocumentRenderer
import caliban.schema._
import caliban.validation.Utils.isObjectType
import caliban.{ Configurator, InputValue }
import zio.prelude._
import zio.prelude.fx.ZPure
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.{ IO, Trace, ZIO }

import scala.annotation.tailrec
import scala.collection.compat._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Validator {

  /**
   * A QueryValidation is a pure program that can access a Context, fail with a ValidationError or succeed with Unit.
   */
  type QueryValidation            = EReader[Context, ValidationError, Unit]
  private type OptionalValidation = Option[EReader[Any, ValidationError, Unit]]

  lazy val AllValidations: List[QueryValidation] =
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
   * Verifies that the given document is valid for this type. Fails with a [[caliban.CalibanError.ValidationError]] otherwise.
   */
  def validate(document: Document, rootType: RootType)(implicit trace: Trace): IO[ValidationError, Unit] =
    Configurator.ref.getWith(v => ZIO.fromEither(check(document, rootType, Map.empty, v.validations).map(_ => ())))

  /**
   * Verifies that the given schema is valid. Fails with a [[caliban.CalibanError.ValidationError]] otherwise.
   */
  def validateSchema[R](schema: RootSchemaBuilder[R])(implicit trace: Trace): IO[ValidationError, RootSchema[R]] =
    ZIO.fromEither(validateSchemaEither(schema))

  def validateSchemaEither[R](schema: RootSchemaBuilder[R]): Either[ValidationError, RootSchema[R]] = {
    val types = schema.types
    ZPure.foreachDiscard(types.sorted)(validateType) *>
      validateClashingTypes(types) *>
      validateDirectives(types) *>
      validateRootMutation(schema) *>
      validateRootSubscription(schema) *>
      validateRootQuery(schema)
  }.runEither

  private val zunit = ZPure.unit[Unit]

  private[caliban] def validateType(t: __Type): EReader[Any, ValidationError, Unit] =
    ZPure.forEach(t.name)(name => checkName(name, s"Type '$name'")) *>
      (t.kind match {
        case __TypeKind.ENUM         => validateEnum(t)
        case __TypeKind.UNION        => validateUnion(t)
        case __TypeKind.INTERFACE    => validateInterface(t)
        case __TypeKind.INPUT_OBJECT => validateInputObject(t)
        case __TypeKind.OBJECT       => validateObject(t)
        case _                       => zunit
      })

  def failValidation(msg: String, explanatoryText: String): EReader[Any, ValidationError, Nothing] =
    ZPure.fail(ValidationError(msg, explanatoryText))

  /**
   * Prepare the request for execution.
   * Fails with a [[caliban.CalibanError.ValidationError]] otherwise.
   *
   * @see [[prepareEither]] for a variant that returns an Either instead
   */
  def prepare[R](
    document: Document,
    rootType: RootType,
    rootSchema: RootSchema[R],
    operationName: Option[String],
    variables: Map[String, InputValue],
    skipValidation: Boolean,
    validations: List[QueryValidation]
  ): IO[ValidationError, ExecutionRequest] = ZIO.fromEither(
    prepareEither(document, rootType, rootSchema, operationName, variables, skipValidation, validations)
  )(Trace.empty)

  /**
   * Prepare the request for execution.
   * Fails with a [[caliban.CalibanError.ValidationError]] otherwise.
   */
  def prepareEither[R](
    document: Document,
    rootType: RootType,
    rootSchema: RootSchema[R],
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
            case _           => Left(ValidationError("Operation name is required.", ""))
          }
      }

      operation.flatMap { op =>
        (op.operationType match {
          case Query        =>
            Right(rootSchema.query)
          case Mutation     =>
            rootSchema.mutation.toRight(ValidationError("Mutations are not supported on this schema", ""))
          case Subscription =>
            rootSchema.subscription.toRight(ValidationError("Subscriptions are not supported on this schema", ""))
        }).map(operation =>
          ExecutionRequest(
            F(
              op.selectionSet,
              fragments,
              variables,
              op.variableDefinitions,
              operation.opType,
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
      operations.foreach(op => collectSelectionSets(buf)(op.selectionSet))
      fragments.foreach(f => collectSelectionSets(buf)(f.selectionSet))
      val context = Context(document, rootType, operations, fragmentMap, buf.result(), variables)
      ZPure.collectAll(validations).provideService[Context](context).runEither.map(_ => fragmentMap)
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

    def collectValues(selectionSet: List[Selection]): Unit = {
      // ugly mutable code but it's worth it for the speed ;)
      def add(args: Map[String, InputValue]): Unit = {
        if (args.nonEmpty) allValues addAll args.values
        ()
      }

      selectionSet.foreach {
        case Field(_, _, arguments, directives, selectionSet, _) =>
          add(arguments)
          directives.foreach(d => add(d.arguments))
          collectValues(selectionSet)
        case FragmentSpread(name, directives)                    =>
          directives.foreach(d => add(d.arguments))
          context.fragments
            .get(name)
            .foreach { f =>
              f.directives.foreach(d => add(d.arguments))
              collectValues(f.selectionSet)
            }
        case InlineFragment(_, directives, selectionSet)         =>
          directives.foreach(d => add(d.arguments))
          collectValues(selectionSet)
      }
    }

    def collectVariableValues(values: Iterable[InputValue]): Unit =
      values.foreach {
        case InputValue.ListValue(values)   => collectVariableValues(values)
        case InputValue.ObjectValue(fields) => collectVariableValues(fields.values)
        case v: VariableValue               => variables.add(v.name)
        case _                              => ()
      }

    collectValues(selectionSet)
    collectVariableValues(allValues)
    variables
  }

  private def collectSelectionSets(
    buffer: ListBuffer[Selection] = ListBuffer.empty
  )(selectionSet: List[Selection]): ListBuffer[Selection] = {
    def loop(selectionSet: List[Selection]): Unit =
      if (selectionSet.nonEmpty) {
        buffer addAll selectionSet
        selectionSet.foreach {
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
  ): EReader[Any, ValidationError, List[(Directive, __DirectiveLocation)]] = {
    val directiveDefinitions = context.document.directiveDefinitions.groupBy(_.name)
    for {
      _                   <- validateAll(context.operations)(op => checkDirectivesUniqueness(op.directives, directiveDefinitions))
      fragmentDirectives   = context.fragments.values.toList.map(_.directives)
      _                   <- validateAll(fragmentDirectives)(checkDirectivesUniqueness(_, directiveDefinitions))
      selectionDirectives <- collectDirectives(context.selectionSets, directiveDefinitions)
    } yield {
      val all = ListBuffer.empty[(Directive, __DirectiveLocation)]
      context.operations.foreach { op =>
        val location = op.operationType match {
          case OperationType.Query        => __DirectiveLocation.QUERY
          case OperationType.Mutation     => __DirectiveLocation.MUTATION
          case OperationType.Subscription => __DirectiveLocation.SUBSCRIPTION
        }
        op.directives.foreach(v => all.addOne((v, location)))
      }
      fragmentDirectives.foreach(_.foreach(v => all.addOne((v, __DirectiveLocation.FRAGMENT_DEFINITION))))
      all.addAll(selectionDirectives)
      all.result()
    }
  }

  private def collectDirectives(
    selectionSet: List[Selection],
    directiveDefinitions: Map[String, List[DirectiveDefinition]]
  ): EReader[Any, ValidationError, List[(Directive, __DirectiveLocation)]] = {
    val builder = ListBuffer.empty[List[(Directive, __DirectiveLocation)]]

    def loop(selectionSet: List[Selection]): Unit =
      selectionSet.foreach {
        case Field(_, _, _, directives, selectionSet, _) =>
          if (directives.nonEmpty)
            builder addOne directives.map((_, __DirectiveLocation.FIELD))
          loop(selectionSet)
        case FragmentSpread(_, directives)               =>
          if (directives.nonEmpty)
            builder addOne directives.map((_, __DirectiveLocation.FRAGMENT_SPREAD))
        case InlineFragment(_, directives, selectionSet) =>
          if (directives.nonEmpty)
            builder addOne directives.map((_, __DirectiveLocation.INLINE_FRAGMENT))
          loop(selectionSet)
      }
    loop(selectionSet)
    val directiveLists                            = builder.result()
    validateAll(directiveLists)(list => checkDirectivesUniqueness(list.map(_._1), directiveDefinitions))
      .as(directiveLists.flatten)
  }

  private def checkDirectivesUniqueness(
    directives: List[Directive],
    directiveDefinitions: Map[String, List[DirectiveDefinition]]
  ): EReader[Any, ValidationError, Unit] =
    directives
      .groupBy(_.name)
      .find { case (n, v) =>
        // only non-repeatable directives count against uniqueness
        // this .lengthCompare is a 2.12 compatible version of .lengthIs and can be replaced after 2.12 support is dropped
        // it's a minor optimization to short-circuit the length check on a List for the off-chance that list is long
        (v.lengthCompare(1) > 0) && !directiveDefinitions.get(n).exists(_.exists(_.isRepeatable))
      } match {
      case None            => zunit
      case Some((name, _)) =>
        failValidation(
          s"Directive '$name' is defined more than once.",
          "Directives are used to describe some metadata or behavioral change on the definition they apply to. When more than one directive of the same name is used, the expected metadata or behavior becomes ambiguous, therefore only one of each non-repeatable directive is allowed per location."
        )
    }

  lazy val validateDirectives: QueryValidation = ZPure.environmentWithPure[Context] { env =>
    val context = env.get[Context]
    for {
      directives <- collectAllDirectives(context)
      _          <- validateAll(directives) { case (d, location) =>
                      (context.rootType.additionalDirectives ::: Introspector.directives).find(_.name == d.name) match {
                        case None            =>
                          failValidation(
                            s"Directive '${d.name}' is not supported.",
                            "GraphQL servers define what directives they support. For each usage of a directive, the directive must be available on that server."
                          )
                        case Some(directive) =>
                          validateAll(d.arguments) { case (arg, argValue) =>
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
  }

  lazy val validateVariables: QueryValidation =
    ZPure.environmentWithPure { env =>
      val context = env.get[Context]
      validateAll(context.operations) { op =>
        val variableDefinitions = op.variableDefinitions
        val variableUsages      = collectVariablesUsed(context, op.selectionSet)
        if (variableDefinitions.isEmpty && variableUsages.isEmpty) zunit
        else
          validateAll(op.variableDefinitions.groupBy(_.name)) { case (name, variables) =>
            failWhen(variables.sizeCompare(1) > 0)(
              s"Variable '$name' is defined more than once.",
              "If any operation defines more than one variable with the same name, it is ambiguous and invalid. It is invalid even if the type of the duplicate variable is the same."
            )
          } *> validateAll(op.variableDefinitions) { v =>
            val t = context.rootType.types.get(Type.innerType(v.variableType))

            val v1 = failWhen(t.map(_.kind).exists {
              case __TypeKind.OBJECT | __TypeKind.UNION | __TypeKind.INTERFACE => true
              case _                                                           => false
            })(
              s"Type of variable '${v.name}' is not a valid input type.",
              "Variables can only be input types. Objects, unions, and interfaces cannot be used as inputs."
            )

            val v2 =
              if (
                t match {
                  case Some(t0) => t0._isOneOfInput
                  case _        => false
                }
              )
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
            validateAll(variableUsages)(v =>
              failWhen(!op.variableDefinitions.exists(_.name == v))(
                s"Variable '$v' is not defined.",
                "Variables are scoped on a per‐operation basis. That means that any variable used within the context of an operation must be defined at the top level of that operation"
              )
            ) *> validateAll(op.variableDefinitions)(v =>
              failWhen(!variableUsages.contains(v.name))(
                s"Variable '${v.name}' is not used.",
                "All variables defined by an operation must be used in that operation or a fragment transitively included by that operation. Unused variables cause a validation error."
              )
            )
          }
      }
    }

  private def collectFragmentSpreads(selectionSet: List[Selection]): List[FragmentSpread] =
    selectionSet.collect { case f: FragmentSpread => f }

  lazy val validateFragmentSpreads: QueryValidation =
    ZPure.environmentWithPure { env =>
      val context = env.get[Context]
      if (context.fragments.isEmpty) zunit
      else {
        val spreads     = collectFragmentSpreads(context.selectionSets)
        val spreadNames = mutable.Set.from(spreads.map(_.name))
        validateAll(context.fragments.values) { f =>
          if (!spreadNames.contains(f.name))
            failValidation(
              s"Fragment '${f.name}' is not used in any spread.",
              "Defined fragments must be used within a document."
            )
          else if (detectCycles(context, f))
            failValidation(
              s"Fragment '${f.name}' forms a cycle.",
              "The graph of fragment spreads must not form any cycles including spreading itself. Otherwise an operation could infinitely spread or infinitely execute on cycles in the underlying data."
            )
          else zunit
        }
      }
    }

  private def detectCycles(context: Context, fragment: FragmentDefinition, visited: Set[String] = Set()): Boolean = {
    val selectionSets     = collectSelectionSets()(fragment.selectionSet).result()
    val descendantSpreads = collectFragmentSpreads(selectionSets)
    descendantSpreads.exists(s =>
      visited.contains(s.name) ||
        context.fragments.get(s.name).fold(false)(f => detectCycles(context, f, visited + s.name))
    )
  }

  lazy val validateDocumentFields: QueryValidation = ZPure.environmentWithPure { env =>
    val context = env.get[Context]
    validateAll(context.document.definitions) {
      case OperationDefinition(opType, _, _, _, selectionSet) =>
        opType match {
          case OperationType.Query        =>
            validateSelectionSet(context, selectionSet, context.rootType.queryType)
          case OperationType.Mutation     =>
            context.rootType.mutationType.fold[EReader[Any, ValidationError, Unit]](
              failValidation("Mutation operations are not supported on this schema.", "")
            )(validateSelectionSet(context, selectionSet, _))
          case OperationType.Subscription =>
            context.rootType.subscriptionType.fold[EReader[Any, ValidationError, Unit]](
              failValidation("Subscription operations are not supported on this schema.", "")
            )(validateSelectionSet(context, selectionSet, _))
        }
      case _: FragmentDefinition                              => zunit
      case _: TypeSystemDefinition                            => zunit
      case _: TypeSystemExtension                             => zunit
    }
  }

  private def containsFragments(selectionSet: List[Selection]): Boolean =
    selectionSet.exists {
      case f: Selection.Field          => containsFragments(f.selectionSet)
      case _: Selection.InlineFragment => true
      case _: Selection.FragmentSpread => true
    }

  private def validateSelectionSet(
    context: Context,
    selectionSet: List[Selection],
    currentType: __Type
  ): EReader[Any, ValidationError, Unit] = {
    val v1 = validateFields(context, selectionSet, currentType)
    if (context.fragments.nonEmpty || containsFragments(selectionSet))
      v1 *> FragmentValidator.findConflictsWithinSelectionSet(context, context.rootType.queryType, selectionSet)
    else v1
  }

  private def validateFields(
    context: Context,
    selectionSet: List[Selection],
    currentType: __Type
  ): EReader[Any, ValidationError, Unit] = {
    val v1 = validateAll(selectionSet) {
      case f: Field                                       =>
        validateField(context, f, currentType)
      case FragmentSpread(name, _)                        =>
        context.fragments.get(name) match {
          case Some(fragment) =>
            validateSpread(context, Some(name), currentType, Some(fragment.typeCondition), fragment.selectionSet)
          case None           =>
            failValidation(
              s"Fragment spread '$name' is not defined.",
              "Named fragment spreads must refer to fragments defined within the document. It is a validation error if the target of a spread is not defined."
            )
        }
      case InlineFragment(typeCondition, _, selectionSet) =>
        validateSpread(context, None, currentType, typeCondition, selectionSet)
    }
    val v2 = validateLeafFieldSelection(currentType, selectionSet.nonEmpty)
    v2.fold(v1)(v1 *> _)
  }

  private def validateSpread(
    context: Context,
    name: Option[String],
    currentType: __Type,
    typeCondition: Option[NamedType],
    selectionSet: List[Selection]
  ): EReader[Any, ValidationError, Unit] =
    typeCondition.fold[Option[__Type]](Some(currentType))(t => context.rootType.types.get(t.name)) match {
      case Some(fragmentType) =>
        val v1 = validateFragmentType(name, fragmentType)
        val v2 = {
          val possibleTypes         = currentType.possibleTypeNames
          val possibleFragmentTypes = fragmentType.possibleTypeNames
          val applicableTypes       = possibleTypes intersect possibleFragmentTypes
          if (applicableTypes.isEmpty)
            failValidation(
              s"${name.fold("Inline fragment spread")(n => s"Fragment spread '$n'")} is not possible: possible types are '${possibleTypes
                  .mkString(", ")}' and possible fragment types are '${possibleFragmentTypes.mkString(", ")}'.",
              "Fragments are declared on a type and will only apply when the runtime object type matches the type condition. They also are spread within the context of a parent type. A fragment spread is only valid if its type condition could ever apply within the parent type."
            )
          else validateFields(context, selectionSet, fragmentType)
        }
        v1.fold(v2)(_ *> v2)
      case None               =>
        val typeConditionName = typeCondition.fold("?")(_.name)
        failValidation(
          s"${name.fold("Inline fragment spread")(n => s"Fragment spread '$n'")} targets an invalid type: '$typeConditionName'.",
          "Fragments must be specified on types that exist in the schema. This applies for both named and inline fragments. If they are not defined in the schema, the query does not validate."
        )
    }

  private def combineOptionalValidations(v1: OptionalValidation, v2: OptionalValidation): OptionalValidation =
    (v1, v2) match {
      case (None, None)         => None
      case (Some(v1), None)     => Some(v1)
      case (None, Some(v2))     => Some(v2)
      case (Some(v1), Some(v2)) => Some(v1 *> v2)
    }

  private def validateField(context: Context, field: Field, currentType: __Type): EReader[Any, ValidationError, Unit] =
    if (field.name != "__typename") {
      currentType.allFieldsMap.get(field.name) match {
        case Some(f) =>
          val v1 = validateFields(context, field.selectionSet, f._type.innerType)
          val v2 = validateArguments(field, f, currentType, context)
          v2.fold(v1)(v1 *> _)
        case None    =>
          failValidation(
            s"Field '${field.name}' does not exist on type '${DocumentRenderer.renderTypeName(currentType)}'.",
            "The target field of a field selection must be defined on the scoped type of the selection set. There are no limitations on alias names."
          )
      }
    } else zunit

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
      val arg0 = field.arguments.get(arg.name)
      val opt1 = (arg.defaultValue, arg0) match {
        case (None, None) | (None, Some(NullValue)) =>
          Some(
            failValidation(
              s"Required argument '${arg.name}' is null or missing on field '${field.name}' of type '${currentType.name
                  .getOrElse("")}'.",
              "Arguments can be required. An argument is required if the argument type is non‐null and does not have a default value. Otherwise, the argument is optional."
            )
          )
        case (Some(_), Some(NullValue))             =>
          Some(
            failValidation(
              s"Required argument '${arg.name}' is null on '${field.name}' of type '${currentType.name
                  .getOrElse("")}'.",
              "Arguments can be required. An argument is required if the argument type is non‐null and does not have a default value. Otherwise, the argument is optional."
            )
          )
        case _                                      => None
      }
      val opt2 =
        if (arg._type.innerType._isOneOfInput)
          Some(
            validateOneOfInputValue(
              arg0.getOrElse(NullValue),
              s"Argument '${arg.name}' on field '${field.name}'"
            )
          )
        else None

      combineOptionalValidations(opt1, opt2)
    })(identity)

    val v2 = validateAllNonEmpty(providedArgs) { case (arg, argValue) =>
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
    }

    combineOptionalValidations(v1, v2)
  }

  private[caliban] def validateInputValues(
    inputValue: __InputValue,
    argValue: InputValue,
    context: Context,
    errorContext: => String
  ): EReader[Any, ValidationError, Unit] = {
    val t           = inputValue._type
    val inputType   = if (t.kind == __TypeKind.NON_NULL) t.ofType.getOrElse(t) else t
    val inputFields = inputType.allInputFields

    argValue match {
      case InputValue.ObjectValue(fields) if inputType.kind == __TypeKind.INPUT_OBJECT =>
        validateAll(fields) { case (k, v) =>
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
        } *> validateAll(inputFields)(inputField =>
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
        context.variableDefinitions.get(variableName) match {
          case Some(variableDefinition) => checkVariableUsageAllowed(variableDefinition, inputValue)
          case None                     =>
            failValidation(
              s"Variable '$variableName' is not defined.",
              "Variables are scoped on a per‐operation basis. That means that any variable used within the context of an operation must be defined at the top level of that operation"
            )
        }
      case _                                                                           => zunit
    }
  } *> ValueValidator.validateInputTypes(inputValue, argValue, context, errorContext)

  private def validateOneOfInputValue(
    inputValue: InputValue,
    errorContext: => String
  ): EReader[Any, ValidationError, Unit] =
    ZPure
      .whenCase(inputValue match {
        case InputValue.ObjectValue(fields) if fields.size == 1 => fields.headOption.map(_._2)
        case vv: InputValue.VariableValue                       => Some(vv)
        case _                                                  => None
      }) { case None | Some(NullValue) =>
        failValidation(
          s"$errorContext is not a valid OneOf Input Object",
          "OneOf Input Object arguments must specify exactly one non-null key"
        )
      }
      .unit

  private def checkVariableUsageAllowed(
    variableDefinition: VariableDefinition,
    inputValue: __InputValue
  ): EReader[Any, ValidationError, Unit] = {
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
  ): EReader[Any, ValidationError, Unit] = {
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

  lazy val validateOperationNameUniqueness: QueryValidation = ZPure.environmentWithPure { env =>
    val context       = env.get[Context]
    val operations    = context.operations
    val names         = operations.flatMap(_.name).groupBy(identity)
    val repeatedNames = names.collect { case (name, items) if items.length > 1 => name }
    failWhen(repeatedNames.nonEmpty)(
      s"Multiple operations have the same name: ${repeatedNames.mkString(", ")}.",
      "Each named operation definition must be unique within a document when referred to by its name."
    )
  }

  lazy val validateLoneAnonymousOperation: QueryValidation = ZPure.environmentWithPure { env =>
    val context    = env.get[Context]
    val operations = context.operations
    val anonymous  = operations.filter(_.name.isEmpty)
    failWhen(operations.length > 1 && anonymous.nonEmpty)(
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

  lazy val validateSubscriptionOperation: QueryValidation = ZPure.environmentWithPure { env =>
    val context = env.get[Context]
    val error   =
      for {
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
      } yield error
    ZPure.fromOption(error).flip
  }

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

  private[caliban] def validateEnum(t: __Type): EReader[Any, ValidationError, Unit] =
    t.allEnumValues match {
      case _ :: _ => zunit
      case Nil    =>
        failValidation(
          s"Enum ${t.name.getOrElse("")} doesn't contain any values",
          "An Enum type must define one or more unique enum values."
        )
    }

  private[caliban] def validateUnion(t: __Type): EReader[Any, ValidationError, Unit] =
    t.possibleTypes match {
      case None | Some(Nil)                           =>
        failValidation(
          s"Union ${t.name.getOrElse("")} doesn't contain any type.",
          "A Union type must include one or more unique member types."
        )
      case Some(types) if !types.forall(isObjectType) =>
        failValidation(
          s"Union ${t.name.getOrElse("")} contains the following non Object types: " +
            types.filterNot(isObjectType).map(_.name.getOrElse("")).filterNot(_.isEmpty).mkString("", ", ", "."),
          s"The member types of a Union type must all be Object base types."
        )
      case _                                          => zunit
    }

  private[caliban] def validateInputObject(t: __Type): EReader[Any, ValidationError, Unit] = {
    lazy val inputObjectContext = s"""${if (t._isOneOfInput) "OneOf " else ""}InputObject '${t.name.getOrElse("")}'"""

    def noDuplicateInputValueName(
      inputValues: List[__InputValue],
      errorContext: => String
    ): EReader[Any, ValidationError, Unit] = {
      val messageBuilder = (i: __InputValue) => s"$errorContext has repeated fields: ${i.name}"
      def explanatory    =
        "The input field must have a unique name within that Input Object type; no two input fields may share the same name"
      noDuplicateName[__InputValue](inputValues, _.name, messageBuilder, explanatory)
    }

    def noDuplicatedOneOfOrigin(inputValues: List[__InputValue]): EReader[Any, ValidationError, Unit] = {
      val resolveOrigin  = (i: __InputValue) =>
        i._parentType.flatMap(_.origin).getOrElse("<unexpected validation error>")
      val messageBuilder = (i: __InputValue) =>
        s"$inputObjectContext is extended by a case class with multiple arguments: ${resolveOrigin(i)}"
      val explanatory    = "All case classes used as arguments to OneOf Input Objects must have exactly one field"
      noDuplicateName[__InputValue](inputValues, resolveOrigin, messageBuilder, explanatory)
    }

    def validateFields(fields: List[__InputValue]): EReader[Any, ValidationError, Unit] =
      validateAll(fields)(validateInputValue(_, inputObjectContext)) *>
        noDuplicateInputValueName(fields, inputObjectContext)

    def validateOneOfFields(fields: List[__InputValue]): EReader[Any, ValidationError, Unit] =
      noDuplicatedOneOfOrigin(fields) *>
        validateAll(fields) { f =>
          failWhen(f.defaultValue.isDefined)(
            s"$inputObjectContext argument has a default value",
            "Fields of OneOf input objects cannot have default values"
          ) *>
            failWhen(!f._type.isNullable)(
              s"$inputObjectContext argument is not nullable",
              "All of OneOf input fields must be declared as nullable in the schema according to the spec"
            )
        }

    t.allInputFields match {
      case Nil                       =>
        failValidation(
          s"$inputObjectContext does not have fields",
          "An Input Object type must define one or more input fields"
        )
      case fields if t._isOneOfInput => validateOneOfFields(fields) *> validateFields(fields)
      case fields                    => validateFields(fields)
    }
  }

  private[caliban] def validateInputValue(
    inputValue: __InputValue,
    errorContext: => String
  ): EReader[Any, ValidationError, Unit] = {
    lazy val fieldContext = s"InputValue '${inputValue.name}' of $errorContext"
    for {
      _ <- ValueValidator.validateDefaultValue(inputValue, fieldContext)
      _ <- checkName(inputValue.name, fieldContext)
      _ <- onlyInputType(inputValue._type, fieldContext)
    } yield ()
  }

  private[caliban] def validateInterface(t: __Type): EReader[Any, ValidationError, Unit] = {
    lazy val interfaceContext = s"Interface '${t.name.getOrElse("")}'"

    t.allFields match {
      case Nil    =>
        failValidation(
          s"$interfaceContext does not have fields",
          "An Interface type must define one or more fields"
        )
      case fields => validateFields(fields, interfaceContext)
    }
  }

  def validateObject(obj: __Type): EReader[Any, ValidationError, Unit] = {
    lazy val objectContext = s"Object '${obj.name.getOrElse("")}'"

    def validateInterfaceFields(obj: __Type) = {
      def fieldNames(t: __Type) = t.allFieldsMap.keySet

      val supertype = obj.interfaces().toList.flatten

      def checkForMissingFields(): EReader[Any, ValidationError, Unit] = {
        val objectFieldNames    = fieldNames(obj)
        val interfaceFieldNames = supertype.map(fieldNames).toSet.flatten
        val isMissingFields     = objectFieldNames.union(interfaceFieldNames) != objectFieldNames

        failWhen(interfaceFieldNames.nonEmpty && isMissingFields)(
          {
            val missingFields = interfaceFieldNames.diff(objectFieldNames).toList.sorted
            s"$objectContext is missing field(s): ${missingFields.mkString(", ")}"
          },
          "An Object type must include a field of the same name for every field defined in an interface"
        )
      }

      def checkForInvalidSubtypeFields(): EReader[Any, ValidationError, Unit] = {
        val objectFields    = obj.allFields
        val supertypeFields = supertype.flatMap(_.allFields)

        def isNonNullableSubtype(supertypeFieldType: __Type, objectFieldType: __Type) = {
          import __TypeKind._
          objectFieldType.kind match {
            case NON_NULL => objectFieldType.ofType.exists(Types.same(supertypeFieldType, _))
            case _        => false
          }
        }

        def isValidSubtype(supertypeFieldType: __Type, objectFieldType: __Type) = {
          val supertypePossibleTypes = supertypeFieldType.possibleTypes.toList.flatten

          Types.same(supertypeFieldType, objectFieldType) ||
          supertypePossibleTypes.exists(Types.same(_, objectFieldType)) ||
          isNonNullableSubtype(supertypeFieldType, objectFieldType)
        }

        validateAll(objectFields) { objField =>
          lazy val fieldContext = s"Field '${objField.name}'"

          supertypeFields.find(_.name == objField.name) match {
            case None             => zunit
            case Some(superField) =>
              val superArgs = superField.allArgs.map(arg => (arg.name, arg)).toMap
              val extraArgs = objField.allArgs.filter { arg =>
                superArgs.get(arg.name).fold(true)(superArg => !Types.same(arg._type, superArg._type))
              }

              def fieldTypeIsValid = isValidSubtype(superField._type, objField._type)

              def listItemTypeIsValid =
                isListField(superField) && isListField(objField) && (for {
                  superListItemType <- superField._type.ofType
                  objListItemType   <- objField._type.ofType
                } yield isValidSubtype(superListItemType, objListItemType)).getOrElse(false)

              def extraArgsAreValid = !extraArgs.exists(_._type.kind == __TypeKind.NON_NULL)

              (fieldTypeIsValid, isListField(superField)) match {
                case (_, true) if !listItemTypeIsValid =>
                  failValidation(
                    s"$fieldContext in $objectContext is an invalid list item subtype",
                    "An object list item field type must be equal to or a possible" +
                      " type of the interface list item field type."
                  )
                case (false, false)                    =>
                  failValidation(
                    s"$fieldContext in $objectContext is an invalid subtype",
                    "An object field type must be equal to or a possible type of the interface field type."
                  )
                case _ if !extraArgsAreValid           =>
                  val argNames = extraArgs.filter(_._type.kind == __TypeKind.NON_NULL).map(_.name).mkString(", ")
                  failValidation(
                    s"$fieldContext with extra non-nullable arg(s) '$argNames' in $objectContext is invalid",
                    "Any additional field arguments must not be of a non-nullable type."
                  )
                case _                                 => zunit
              }
          }
        }
      }

      for {
        _ <- checkForMissingFields()
        _ <- checkForInvalidSubtypeFields()
      } yield ()
    }

    obj.allFields match {
      case Nil    =>
        failValidation(
          s"$objectContext does not have fields",
          "An Object type must define one or more fields"
        )
      case fields => validateFields(fields, objectContext) *> validateInterfaceFields(obj)
    }
  }

  private def isListField(field: __Field) =
    field._type.kind == __TypeKind.LIST

  private[caliban] def onlyInputType(`type`: __Type, errorContext: => String): EReader[Any, ValidationError, Unit] = {
    // https://spec.graphql.org/June2018/#IsInputType()
    def isInputType(t: __Type): Either[__Type, Unit] = {
      import __TypeKind._
      t.kind match {
        case LIST | NON_NULL              => t.ofType.fold[Either[__Type, Unit]](Left(t))(isInputType)
        case SCALAR | ENUM | INPUT_OBJECT => Right(())
        case _                            => Left(t)
      }
    }

    isInputType(`type`) match {
      case Left(errorType) =>
        failValidation(
          s"${errorType.name.getOrElse("")} of $errorContext is of kind ${errorType.kind}, must be an InputType",
          """The input field must accept a type where IsInputType(type) returns true, https://spec.graphql.org/June2018/#IsInputType()"""
        )
      case Right(_)        => zunit
    }
  }

  private[caliban] def validateFields(fields: List[__Field], context: => String): EReader[Any, ValidationError, Unit] =
    noDuplicateFieldName(fields, context) <*
      validateAll(fields) { field =>
        lazy val fieldContext = s"Field '${field.name}' of $context"
        for {
          _ <- checkName(field.name, fieldContext)
          _ <- onlyOutputType(field._type, fieldContext)
          _ <- validateAll(field.allArgs)(validateInputValue(_, fieldContext))
        } yield ()
      }

  private[caliban] def noDuplicateFieldName(fields: List[__Field], errorContext: => String) = {
    val messageBuilder = (f: __Field) => s"$errorContext has repeated fields: ${f.name}"
    def explanatory    =
      "The field must have a unique name within that Interface type; no two fields may share the same name"
    noDuplicateName[__Field](fields, _.name, messageBuilder, explanatory)
  }

  private[caliban] def onlyOutputType(`type`: __Type, errorContext: => String): EReader[Any, ValidationError, Unit] = {
    // https://spec.graphql.org/June2018/#IsOutputType()
    def isOutputType(t: __Type): Either[__Type, Unit] = {
      import __TypeKind._
      t.kind match {
        case LIST | NON_NULL                            => t.ofType.fold[Either[__Type, Unit]](Left(t))(isOutputType)
        case SCALAR | OBJECT | INTERFACE | UNION | ENUM => Right(())
        case _                                          => Left(t)
      }
    }

    isOutputType(`type`) match {
      case Left(errorType) =>
        failValidation(
          s"${errorType.name.getOrElse("")} of $errorContext is of kind ${errorType.kind}, must be an OutputType",
          """The input field must accept a type where IsOutputType(type) returns true, https://spec.graphql.org/June2018/#IsInputType()"""
        )
      case Right(_)        => zunit
    }
  }

  private[caliban] def noDuplicateName[T](
    listOfNamed: List[T],
    nameExtractor: T => String,
    messageBuilder: T => String,
    explanatoryText: => String
  ): EReader[Any, ValidationError, Unit] =
    listOfNamed
      .groupBy(nameExtractor(_))
      .collectFirst { case (_, f :: _ :: _) => f }
      .fold[EReader[Any, ValidationError, Unit]](zunit)(duplicate =>
        failValidation(messageBuilder(duplicate), explanatoryText)
      )

  private[caliban] def checkName(name: String, fieldContext: => String): EReader[Any, ValidationError, Unit] =
    ZPure
      .fromEither(Parser.parseName(name))
      .mapError(e =>
        ValidationError(
          s"$fieldContext is not a valid name.",
          s"Name does not conform to the GraphQL spec for names: ${e.msg}"
        )
      ) *> doesNotStartWithUnderscore(name, fieldContext)

  private[caliban] def doesNotStartWithUnderscore(
    name: String,
    errorContext: => String
  ): EReader[Any, ValidationError, Unit] =
    failWhen(name.startsWith("__"))(
      s"$errorContext can't start with '__'",
      """Names can not begin with the characters "__" (two underscores)"""
    )

  private[caliban] def validateRootQuery[R](
    schema: RootSchemaBuilder[R]
  ): EReader[Any, ValidationError, RootSchema[R]] =
    schema.query match {
      case None        =>
        failValidation(
          "The query root operation is missing.",
          "The query root operation type must be provided and must be an Object type."
        )
      case Some(query) =>
        if (query.opType.kind == __TypeKind.OBJECT)
          ZPure.succeed(RootSchema(query, schema.mutation, schema.subscription))
        else
          failValidation(
            "The query root operation is not an object type.",
            "The query root operation type must be provided and must be an Object type."
          )
    }

  private[caliban] def validateRootMutation[R](schema: RootSchemaBuilder[R]): EReader[Any, ValidationError, Unit] =
    schema.mutation match {
      case Some(mutation) if mutation.opType.kind != __TypeKind.OBJECT =>
        failValidation(
          "The mutation root operation is not an object type.",
          "The mutation root operation type is optional; if it is not provided, the service does not support mutations. If it is provided, it must be an Object type."
        )
      case _                                                           => zunit
    }

  private[caliban] def validateRootSubscription[R](schema: RootSchemaBuilder[R]): EReader[Any, ValidationError, Unit] =
    schema.subscription match {
      case Some(subscription) if subscription.opType.kind != __TypeKind.OBJECT =>
        failValidation(
          "The mutation root subscription is not an object type.",
          "The mutation root subscription type is optional; if it is not provided, the service does not support subscriptions. If it is provided, it must be an Object type."
        )
      case _                                                                   => zunit
    }

  private[caliban] def validateClashingTypes(types: List[__Type]): EReader[Any, ValidationError, Unit] = {
    val check = types.groupBy(_.name).collectFirst { case (Some(name), v) if v.size > 1 => (name, v) }
    check match {
      case None                 => zunit
      case Some((name, values)) =>
        failValidation(
          s"Type '$name' is defined multiple times (${values
              .sortBy(v => v.origin.getOrElse(""))
              .map(v => s"${v.kind}${v.origin.fold("")(a => s" in $a")}")
              .mkString(", ")}).",
          "Each type must be defined only once."
        )
    }
  }

  private def validateDirectives(types: List[__Type]): EReader[Any, ValidationError, Unit] = {

    def validateArguments(
      args: Map[String, InputValue],
      errorContext: => String
    ): EReader[Any, ValidationError, Unit] = {
      val argumentErrorContextBuilder = (name: String) => s"Argument '$name' of $errorContext"
      validateAll(args.keys)(argName => checkName(argName, argumentErrorContextBuilder(argName)))
    }

    def validateDirective(directive: Directive, errorContext: => String) = {
      lazy val directiveErrorContext = s"Directive '${directive.name}' of $errorContext"

      checkName(directive.name, directiveErrorContext) *>
        validateArguments(directive.arguments, directiveErrorContext)
    }

    def validateDirectives(
      directives: Option[List[Directive]],
      errorContext: => String
    ): EReader[Any, ValidationError, Unit] =
      validateAll(directives.getOrElse(List.empty))(validateDirective(_, errorContext))

    def validateInputValueDirectives(
      inputValues: List[__InputValue],
      errorContext: => String
    ): EReader[Any, ValidationError, Unit] = {
      val inputValueErrorContextBuilder = (name: String) => s"InputValue '$name' of $errorContext"
      validateAll(inputValues)(iv => validateDirectives(iv.directives, inputValueErrorContextBuilder(iv.name)))
    }

    def validateFieldDirectives(
      field: __Field,
      errorContext: => String
    ): EReader[Any, ValidationError, Unit] = {
      lazy val fieldErrorContext = s"Field '${field.name}' of $errorContext"
      validateDirectives(field.directives, fieldErrorContext) *>
        validateInputValueDirectives(field.allArgs, fieldErrorContext)
    }

    validateAll(types) { t =>
      lazy val typeErrorContext = s"Type '${t.name.getOrElse("")}'"
      for {
        _ <- validateDirectives(t.directives, typeErrorContext)
        _ <- validateInputValueDirectives(t.allInputFields, typeErrorContext)
        _ <- validateAll(t.allFields)(validateFieldDirectives(_, typeErrorContext))
      } yield ()
    }
  }

  /**
   * Wrapper around `ZPure.foreachDiscard` optimized for cases where the input is empty or has only one element.
   */
  private def validateAll[R, A](
    in: Iterable[A]
  )(f: A => EReader[R, ValidationError, Unit]): EReader[R, ValidationError, Unit] =
    in.sizeCompare(1) match {
      case -1 => zunit
      case 0  => f(in.head)
      case _  => ZPure.foreachDiscard(in)(f)
    }

  private def validateAllNonEmpty[A](
    in: Iterable[A]
  )(f: A => EReader[Any, ValidationError, Unit]): OptionalValidation =
    in.sizeCompare(1) match {
      case -1 => None
      case 0  => Some(f(in.head))
      case _  => Some(ZPure.foreachDiscard(in)(f))
    }

  private def failWhen(
    condition: Boolean
  )(msg: => String, explanatoryText: => String): EReader[Any, ValidationError, Unit] =
    if (condition) failValidation(msg, explanatoryText) else zunit

  private implicit class EnrichedListBufferOps[A](private val lb: ListBuffer[A]) extends AnyVal {
    // This method doesn't exist in Scala 2.12 so we just use `.map` for it instead
    def addOne(elem: A): ListBuffer[A]            = lb += elem
    def addAll(elems: Iterable[A]): ListBuffer[A] = lb ++= elems
  }
}
