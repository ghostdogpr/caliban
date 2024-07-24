package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.InputValue
import caliban.introspection.adt.__TypeKind._
import caliban.introspection.adt.{ __Field, __InputValue, __Type, __TypeKind }
import caliban.parsing.Parser
import caliban.parsing.adt.Directive
import caliban.schema.{ RootSchema, RootSchemaBuilder, Types }
import caliban.validation.Utils.isObjectType
import caliban.validation.ValidationOps._

/*
 * TODO In the next major version:
 *  1. Make `SchemaValidator` an object and remove inheritance from Validator
 *  2. Make all methods private except `validateSchema` and `validateType`
 */
private[caliban] trait SchemaValidator {

  /**
   * Verifies that the given schema is valid. Fails with a [[caliban.CalibanError.ValidationError]] otherwise.
   */
  def validateSchema[R](schema: RootSchemaBuilder[R]): Either[ValidationError, RootSchema[R]] = {
    val types = schema.types.sorted
    for {
      _      <- validateAllDiscard(types)(validateType)
      _      <- validateClashingTypes(types)
      _      <- validateDirectives(types)
      _      <- validateRootMutation(schema)
      _      <- validateRootSubscription(schema)
      schema <- validateRootQuery(schema)
    } yield schema
  }

  private[caliban] def validateType(t: __Type): Either[ValidationError, Unit] =
    t.name.fold[Either[ValidationError, Unit]](unit)(name => checkName(name, s"Type '$name'")) *>
      (t.kind match {
        case __TypeKind.ENUM         => validateEnum(t)
        case __TypeKind.UNION        => validateUnion(t)
        case __TypeKind.INTERFACE    => validateInterface(t)
        case __TypeKind.INPUT_OBJECT => validateInputObject(t)
        case __TypeKind.OBJECT       => validateObject(t)
        case _                       => unit
      })

  private[caliban] def validateClashingTypes(types: List[__Type]): Either[ValidationError, Unit] = {
    val check = types.groupBy(_.name).collectFirst { case (Some(name), v) if v.size > 1 => (name, v) }
    check match {
      case None                 => unit
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

  private def validateDirectives(types: List[__Type]): Either[ValidationError, Unit] = {

    def validateArguments(
      args: Map[String, InputValue],
      errorContext: => String
    ): Either[ValidationError, Unit] = {
      val argumentErrorContextBuilder = (name: String) => s"Argument '$name' of $errorContext"
      validateAllDiscard(args.keys.toList)(argName => checkName(argName, argumentErrorContextBuilder(argName)))
    }

    def validateDirective(directive: Directive, errorContext: => String) = {
      lazy val directiveErrorContext = s"Directive '${directive.name}' of $errorContext"

      checkName(directive.name, directiveErrorContext) *>
        validateArguments(directive.arguments, directiveErrorContext)
    }

    def validateDirectives(
      directives: Option[List[Directive]],
      errorContext: => String
    ): Either[ValidationError, Unit] =
      validateAllDiscard(directives.getOrElse(List.empty))(validateDirective(_, errorContext))

    def validateInputValueDirectives(
      inputValues: List[__InputValue],
      errorContext: => String
    ): Either[ValidationError, Unit] = {
      val inputValueErrorContextBuilder = (name: String) => s"InputValue '$name' of $errorContext"
      validateAllDiscard(inputValues)(iv => validateDirectives(iv.directives, inputValueErrorContextBuilder(iv.name)))
    }

    def validateFieldDirectives(
      field: __Field,
      errorContext: => String
    ): Either[ValidationError, Unit] = {
      lazy val fieldErrorContext = s"Field '${field.name}' of $errorContext"
      validateDirectives(field.directives, fieldErrorContext) *>
        validateInputValueDirectives(field.allArgs, fieldErrorContext)
    }

    validateAllDiscard(types) { t =>
      lazy val typeErrorContext = s"Type '${t.name.getOrElse("")}'"
      for {
        _ <- validateDirectives(t.directives, typeErrorContext)
        _ <- validateInputValueDirectives(t.allInputFields, typeErrorContext)
        _ <- validateAllDiscard(t.allFields)(validateFieldDirectives(_, typeErrorContext))
      } yield ()
    }
  }

  private[caliban] def validateEnum(t: __Type): Either[ValidationError, Unit] =
    t.allEnumValues match {
      case _ :: _ => unit
      case Nil    =>
        failValidation(
          s"Enum ${t.name.getOrElse("")} doesn't contain any values",
          "An Enum type must define one or more unique enum values."
        )
    }

  private[caliban] def validateUnion(t: __Type): Either[ValidationError, Unit] =
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
      case _                                          => unit
    }

  private[caliban] def validateInputObject(t: __Type): Either[ValidationError, Unit] = {
    lazy val inputObjectContext = s"""${if (t._isOneOfInput) "OneOf " else ""}InputObject '${t.name.getOrElse("")}'"""

    def noDuplicateInputValueName(
      inputValues: List[__InputValue],
      errorContext: => String
    ): Either[ValidationError, Unit] = {
      val messageBuilder = (i: __InputValue) => s"$errorContext has repeated fields: ${i.name}"
      def explanatory    =
        "The input field must have a unique name within that Input Object type; no two input fields may share the same name"
      noDuplicateName[__InputValue](inputValues, _.name, messageBuilder, explanatory)
    }

    def noDuplicatedOneOfOrigin(inputValues: List[__InputValue]): Either[ValidationError, Unit] = {
      val resolveOrigin  = (i: __InputValue) =>
        i._parentType.flatMap(_.origin).getOrElse("<unexpected validation error>")
      val messageBuilder = (i: __InputValue) =>
        s"$inputObjectContext is extended by a case class with multiple arguments: ${resolveOrigin(i)}"
      val explanatory    = "All case classes used as arguments to OneOf Input Objects must have exactly one field"
      noDuplicateName[__InputValue](inputValues, resolveOrigin, messageBuilder, explanatory)
    }

    def validateFields(fields: List[__InputValue]): Either[ValidationError, Unit] =
      validateAllDiscard(fields)(validateInputValue(_, inputObjectContext)) *>
        noDuplicateInputValueName(fields, inputObjectContext)

    def validateOneOfFields(fields: List[__InputValue]): Either[ValidationError, Unit] =
      noDuplicatedOneOfOrigin(fields) *>
        validateAllDiscard(fields) { f =>
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
  ): Either[ValidationError, Unit] = {
    lazy val fieldContext = s"InputValue '${inputValue.name}' of $errorContext"
    for {
      _ <- ValueValidator.validateDefaultValue(inputValue, fieldContext)
      _ <- checkName(inputValue.name, fieldContext)
      _ <- onlyInputType(inputValue._type, fieldContext)
    } yield ()
  }

  private[caliban] def validateInterface(t: __Type): Either[ValidationError, Unit] = {
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

  def validateObject(obj: __Type): Either[ValidationError, Unit] = {
    lazy val objectContext = s"Object '${obj.name.getOrElse("")}'"

    def validateInterfaceFields(obj: __Type) = {
      def fieldNames(t: __Type) = t.allFields.map(_.name)

      val supertype = obj.interfaces().toList.flatten

      def checkForMissingFields(): Either[ValidationError, Unit] = {
        val objectFieldNames    = fieldNames(obj).toSet
        val interfaceFieldNames = supertype.flatMap(fieldNames).toSet
        val isMissingFields     = objectFieldNames.union(interfaceFieldNames) != objectFieldNames

        failWhen(interfaceFieldNames.nonEmpty && isMissingFields)(
          {
            val missingFields = interfaceFieldNames.diff(objectFieldNames).toList.sorted
            s"$objectContext is missing field(s): ${missingFields.mkString(", ")}"
          },
          "An Object type must include a field of the same name for every field defined in an interface"
        )
      }

      def checkForInvalidSubtypeFields(): Either[ValidationError, Unit] = {
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

        validateAllDiscard(objectFields) { objField =>
          lazy val fieldContext = s"Field '${objField.name}'"

          supertypeFields.find(_.name == objField.name) match {
            case None             => unit
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
                case _                                 => unit
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

  private[caliban] def onlyInputType(`type`: __Type, errorContext: => String): Either[ValidationError, Unit] = {
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
      case Right(_)        => unit
    }
  }

  private[caliban] def validateFields(fields: List[__Field], context: => String): Either[ValidationError, Unit] =
    noDuplicateFieldName(fields, context) *>
      validateAllDiscard(fields) { field =>
        lazy val fieldContext = s"Field '${field.name}' of $context"
        for {
          _ <- checkName(field.name, fieldContext)
          _ <- onlyOutputType(field._type, fieldContext)
          _ <- validateAllDiscard(field.allArgs)(validateInputValue(_, fieldContext))
        } yield ()
      }

  private[caliban] def noDuplicateFieldName(fields: List[__Field], errorContext: => String) = {
    val messageBuilder = (f: __Field) => s"$errorContext has repeated fields: ${f.name}"
    def explanatory    =
      "The field must have a unique name within that Interface type; no two fields may share the same name"
    noDuplicateName[__Field](fields, _.name, messageBuilder, explanatory)
  }

  private[caliban] def onlyOutputType(`type`: __Type, errorContext: => String): Either[ValidationError, Unit] = {
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
      case Right(_)        => unit
    }
  }

  private[caliban] def noDuplicateName[T](
    listOfNamed: List[T],
    nameExtractor: T => String,
    messageBuilder: T => String,
    explanatoryText: => String
  ): Either[ValidationError, Unit] =
    listOfNamed
      .groupBy(nameExtractor(_))
      .collectFirst { case (_, f :: _ :: _) => f }
      .fold[Either[ValidationError, Unit]](unit)(duplicate =>
        failValidation(messageBuilder(duplicate), explanatoryText)
      )

  private[caliban] def checkName(name: String, fieldContext: => String): Either[ValidationError, Unit] =
    Parser
      .parseName(name)
      .left
      .map(e =>
        ValidationError(
          s"$fieldContext is not a valid name.",
          s"Name does not conform to the GraphQL spec for names: ${e.msg}"
        )
      ) *> doesNotStartWithUnderscore(name, fieldContext)

  private[caliban] def doesNotStartWithUnderscore(
    name: String,
    errorContext: => String
  ): Either[ValidationError, Unit] =
    failWhen(name.startsWith("__"))(
      s"$errorContext can't start with '__'",
      """Names can not begin with the characters "__" (two underscores)"""
    )

  private[caliban] def validateRootQuery[R](
    schema: RootSchemaBuilder[R]
  ): Either[ValidationError, RootSchema[R]] =
    schema.query match {
      case None        =>
        failValidation(
          "The query root operation is missing.",
          "The query root operation type must be provided and must be an Object type."
        )
      case Some(query) =>
        if (query.opType.kind == __TypeKind.OBJECT)
          Right(RootSchema(query, schema.mutation, schema.subscription))
        else
          failValidation(
            "The query root operation is not an object type.",
            "The query root operation type must be provided and must be an Object type."
          )
    }

  private[caliban] def validateRootMutation[R](schema: RootSchemaBuilder[R]): Either[ValidationError, Unit] =
    schema.mutation match {
      case Some(mutation) if mutation.opType.kind != __TypeKind.OBJECT =>
        failValidation(
          "The mutation root operation is not an object type.",
          "The mutation root operation type is optional; if it is not provided, the service does not support mutations. If it is provided, it must be an Object type."
        )
      case _                                                           => unit
    }

  private[caliban] def validateRootSubscription[R](schema: RootSchemaBuilder[R]): Either[ValidationError, Unit] =
    schema.subscription match {
      case Some(subscription) if subscription.opType.kind != __TypeKind.OBJECT =>
        failValidation(
          "The mutation root subscription is not an object type.",
          "The mutation root subscription type is optional; if it is not provided, the service does not support subscriptions. If it is provided, it must be an Object type."
        )
      case _                                                                   => unit
    }

  private def failValidation(msg: String, explanatoryText: String): Either[ValidationError, Nothing] =
    Left(ValidationError(msg, explanatoryText))

}
