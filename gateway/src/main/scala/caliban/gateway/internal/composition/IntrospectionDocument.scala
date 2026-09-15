package caliban.gateway.internal.composition

import caliban.{ InputValue, ResponseValue }
import caliban.parsing.adt.{ Directive, Document, Type }
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveLocation._
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.Parser
import caliban.parsing.SourceMapper
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ BooleanValue, NullValue, StringValue }

private[gateway] object IntrospectionDocument {

  val OperationName = "__CalibanGatewayIntrospection"

  val Query: String =
    s"""query $OperationName {
       |  __schema {
       |    queryType { name }
       |    mutationType { name }
       |    subscriptionType { name }
       |    types { ...FullType }
       |    directives {
       |      name
       |      description
       |      locations
       |      args(includeDeprecated: true) { ...InputValue }
       |      isRepeatable
       |    }
       |  }
       |}
       |fragment FullType on __Type {
       |  kind
       |  name
       |  description
       |  fields(includeDeprecated: true) {
       |    name
       |    description
       |    args(includeDeprecated: true) { ...InputValue }
       |    type { ...TypeRef }
       |    isDeprecated
       |    deprecationReason
       |  }
       |  inputFields(includeDeprecated: true) { ...InputValue }
       |  interfaces { ...TypeRef }
       |  enumValues(includeDeprecated: true) {
       |    name
       |    description
       |    isDeprecated
       |    deprecationReason
       |  }
       |  possibleTypes { ...TypeRef }
       |}
       |fragment InputValue on __InputValue {
       |  name
       |  description
       |  type { ...TypeRef }
       |  defaultValue
       |  isDeprecated
       |  deprecationReason
       |}
       |fragment TypeRef on __Type {
       |  kind
       |  name
       |  ofType {
       |    kind
       |    name
       |    ofType {
       |      kind
       |      name
       |      ofType {
       |        kind
       |        name
       |        ofType {
       |          kind
       |          name
       |          ofType {
       |            kind
       |            name
       |            ofType {
       |              kind
       |              name
       |              ofType {
       |                kind
       |                name
       |                ofType {
       |                  kind
       |                  name
       |                }
       |              }
       |            }
       |          }
       |        }
       |      }
       |    }
       |  }
       |}""".stripMargin

  final case class Invalid(path: String) extends Exception(s"Unexpected introspection response shape at '$path'.")

  def decode(data: ResponseValue): Either[Invalid, Document] =
    for {
      schema       <- objectField(data, "__schema", "__schema")
      queryType    <- optionalName(schema, "queryType")
      mutation     <- optionalName(schema, "mutationType")
      subscription <- optionalName(schema, "subscriptionType")
      types        <- list(schema, "types", "__schema.types")(fullType)
      directives   <- list(schema, "directives", "__schema.directives")(directive)
    } yield {
      val definition = SchemaDefinition(Nil, queryType, mutation, subscription, None)
      val kept       = types.flatten.filterNot(_.name.startsWith("__"))
      Document(definition :: kept ++ directives, SourceMapper.empty)
    }

  private def optionalName(schema: ObjectValue, field: String): Either[Invalid, Option[String]] =
    schema.getOrNull(field) match {
      case null | NullValue   => Right(None)
      case value: ObjectValue => optionalString(value, "name", s"__schema.$field.name")
      case _                  => Left(Invalid(s"__schema.$field"))
    }

  private def fullType(value: ResponseValue, path: String): Either[Invalid, Option[TypeDefinition]] =
    for {
      obj           <- asObject(value, path)
      kind          <- string(obj, "kind", path)
      name          <- optionalString(obj, "name", path).map(_.getOrElse(""))
      description   <- optionalString(obj, "description", path)
      fields        <- optionalList(obj, "fields", s"$path.fields")(field).map(_.getOrElse(Nil))
      inputFields   <- optionalList(obj, "inputFields", s"$path.inputFields")(inputValue).map(_.getOrElse(Nil))
      interfaces    <- optionalList(obj, "interfaces", s"$path.interfaces")(typeRef).map(_.getOrElse(Nil))
      enumValues    <- optionalList(obj, "enumValues", s"$path.enumValues")(enumValue).map(_.getOrElse(Nil))
      possibleTypes <- optionalList(obj, "possibleTypes", s"$path.possibleTypes")(typeRef).map(_.getOrElse(Nil))
      definition    <- kind match {
                         case "SCALAR"            => Right(Some(ScalarTypeDefinition(description, name, Nil)))
                         case "OBJECT"            =>
                           Right(Some(ObjectTypeDefinition(description, name, namedTypes(interfaces), Nil, fields)))
                         case "INTERFACE"         =>
                           Right(Some(InterfaceTypeDefinition(description, name, namedTypes(interfaces), Nil, fields)))
                         case "UNION"             =>
                           Right(Some(UnionTypeDefinition(description, name, Nil, namedTypes(possibleTypes).map(_.name))))
                         case "ENUM"              => Right(Some(EnumTypeDefinition(description, name, Nil, enumValues)))
                         case "INPUT_OBJECT"      => Right(Some(InputObjectTypeDefinition(description, name, Nil, inputFields)))
                         case "LIST" | "NON_NULL" => Right(None)
                         case _                   => Left(Invalid(s"$path.kind"))
                       }
    } yield definition

  private def namedTypes(types: List[Type]): List[NamedType] = types.collect { case t: NamedType => t }

  private def field(value: ResponseValue, path: String): Either[Invalid, FieldDefinition] =
    for {
      obj         <- asObject(value, path)
      name        <- string(obj, "name", path)
      description <- optionalString(obj, "description", path)
      args        <- optionalList(obj, "args", s"$path.args")(inputValue).map(_.getOrElse(Nil))
      tpe         <- objectField(obj, "type", s"$path.type").flatMap(typeRef(_, s"$path.type"))
      deprecation <- deprecationDirectives(obj, path)
    } yield FieldDefinition(description, name, args, tpe, deprecation)

  private def inputValue(value: ResponseValue, path: String): Either[Invalid, InputValueDefinition] =
    for {
      obj         <- asObject(value, path)
      name        <- string(obj, "name", path)
      description <- optionalString(obj, "description", path)
      tpe         <- objectField(obj, "type", s"$path.type").flatMap(typeRef(_, s"$path.type"))
      default     <- optionalString(obj, "defaultValue", path)
      deprecation <- deprecationDirectives(obj, path)
    } yield InputValueDefinition(
      description,
      name,
      tpe,
      default.flatMap(raw => Parser.parseInputValue(raw).toOption),
      deprecation
    )

  private def enumValue(value: ResponseValue, path: String): Either[Invalid, EnumValueDefinition] =
    for {
      obj         <- asObject(value, path)
      name        <- string(obj, "name", path)
      description <- optionalString(obj, "description", path)
      deprecation <- deprecationDirectives(obj, path)
    } yield EnumValueDefinition(description, name, deprecation)

  private def directive(value: ResponseValue, path: String): Either[Invalid, DirectiveDefinition] =
    for {
      obj         <- asObject(value, path)
      name        <- string(obj, "name", path)
      description <- optionalString(obj, "description", path)
      locations   <- list(obj, "locations", s"$path.locations")((location, locationPath) =>
                       location match {
                         case StringValue(name) => directiveLocation(name).toRight(Invalid(locationPath))
                         case _                 => Left(Invalid(locationPath))
                       }
                     )
      args        <- optionalList(obj, "args", s"$path.args")(inputValue).map(_.getOrElse(Nil))
      repeatable  <- optionalBoolean(obj, "isRepeatable", path)
    } yield DirectiveDefinition(description, name, args, repeatable, locations.toSet)

  private def typeRef(value: ResponseValue, path: String): Either[Invalid, Type] =
    asObject(value, path).flatMap { obj =>
      string(obj, "kind", path).flatMap {
        case "NON_NULL" =>
          nested(obj, path).map {
            case NamedType(name, _)  => NamedType(name, nonNull = true)
            case ListType(ofType, _) => ListType(ofType, nonNull = true)
          }
        case "LIST"     => nested(obj, path).map(ListType(_, nonNull = false))
        case _          => optionalString(obj, "name", path).map(name => NamedType(name.getOrElse(""), nonNull = false))
      }
    }

  private def nested(obj: ObjectValue, path: String): Either[Invalid, Type] =
    obj.getOrNull("ofType") match {
      case null | NullValue => Right(NamedType("", nonNull = false))
      case value            => typeRef(value, s"$path.ofType")
    }

  private def deprecationDirectives(obj: ObjectValue, path: String): Either[Invalid, List[Directive]] =
    for {
      deprecated <- optionalBoolean(obj, "isDeprecated", path)
      reason     <- optionalString(obj, "deprecationReason", path)
    } yield
      if (!deprecated) Nil
      else
        List(
          Directive(
            "deprecated",
            reason.fold(Map.empty[String, InputValue])(value => Map("reason" -> StringValue(value)))
          )
        )

  private def directiveLocation(name: String): Option[DirectiveLocation] =
    name match {
      case "QUERY"                  => Some(ExecutableDirectiveLocation.QUERY)
      case "MUTATION"               => Some(ExecutableDirectiveLocation.MUTATION)
      case "SUBSCRIPTION"           => Some(ExecutableDirectiveLocation.SUBSCRIPTION)
      case "FIELD"                  => Some(ExecutableDirectiveLocation.FIELD)
      case "FRAGMENT_DEFINITION"    => Some(ExecutableDirectiveLocation.FRAGMENT_DEFINITION)
      case "FRAGMENT_SPREAD"        => Some(ExecutableDirectiveLocation.FRAGMENT_SPREAD)
      case "INLINE_FRAGMENT"        => Some(ExecutableDirectiveLocation.INLINE_FRAGMENT)
      case "SCHEMA"                 => Some(TypeSystemDirectiveLocation.SCHEMA)
      case "SCALAR"                 => Some(TypeSystemDirectiveLocation.SCALAR)
      case "OBJECT"                 => Some(TypeSystemDirectiveLocation.OBJECT)
      case "FIELD_DEFINITION"       => Some(TypeSystemDirectiveLocation.FIELD_DEFINITION)
      case "ARGUMENT_DEFINITION"    => Some(TypeSystemDirectiveLocation.ARGUMENT_DEFINITION)
      case "INTERFACE"              => Some(TypeSystemDirectiveLocation.INTERFACE)
      case "UNION"                  => Some(TypeSystemDirectiveLocation.UNION)
      case "ENUM"                   => Some(TypeSystemDirectiveLocation.ENUM)
      case "ENUM_VALUE"             => Some(TypeSystemDirectiveLocation.ENUM_VALUE)
      case "INPUT_OBJECT"           => Some(TypeSystemDirectiveLocation.INPUT_OBJECT)
      case "INPUT_FIELD_DEFINITION" => Some(TypeSystemDirectiveLocation.INPUT_FIELD_DEFINITION)
      case "VARIABLE_DEFINITION"    => Some(TypeSystemDirectiveLocation.VARIABLE_DEFINITION)
      case _                        => None
    }

  private[composition] def asObject(value: ResponseValue, path: String): Either[Invalid, ObjectValue] =
    value match {
      case obj: ObjectValue => Right(obj)
      case _                => Left(Invalid(path))
    }

  private def objectField(value: ResponseValue, field: String, path: String): Either[Invalid, ObjectValue] =
    value match {
      case obj: ObjectValue =>
        obj.getOrNull(field) match {
          case nested: ObjectValue => Right(nested)
          case _                   => Left(Invalid(path))
        }
      case _                => Left(Invalid(path))
    }

  private def string(obj: ObjectValue, field: String, path: String): Either[Invalid, String] =
    obj.getOrNull(field) match {
      case StringValue(value) => Right(value)
      case _                  => Left(Invalid(s"$path.$field"))
    }

  private def optionalString(obj: ObjectValue, field: String, path: String): Either[Invalid, Option[String]] =
    obj.getOrNull(field) match {
      case null | NullValue   => Right(None)
      case StringValue(value) => Right(Some(value))
      case _                  => Left(Invalid(s"$path.$field"))
    }

  private def optionalBoolean(obj: ObjectValue, field: String, path: String): Either[Invalid, Boolean] =
    obj.getOrNull(field) match {
      case null | NullValue    => Right(false)
      case BooleanValue(value) => Right(value)
      case _                   => Left(Invalid(s"$path.$field"))
    }

  private def list[A](obj: ObjectValue, field: String, path: String)(
    read: (ResponseValue, String) => Either[Invalid, A]
  ): Either[Invalid, List[A]] =
    optionalList(obj, field, path)(read).flatMap(_.toRight(Invalid(path)))

  private def optionalList[A](obj: ObjectValue, field: String, path: String)(
    read: (ResponseValue, String) => Either[Invalid, A]
  ): Either[Invalid, Option[List[A]]] =
    obj.getOrNull(field) match {
      case null | NullValue => Right(None)
      case ListValue(items) =>
        val builder          = List.newBuilder[A]
        var index            = 0
        var failure: Invalid = null
        var remaining        = items
        while ((remaining ne Nil) && (failure eq null)) {
          read(remaining.head, s"$path[$index]") match {
            case Right(item) => builder += item
            case Left(error) => failure = error
          }
          index += 1
          remaining = remaining.tail
        }
        if (failure eq null) Right(Some(builder.result())) else Left(failure)
      case _                => Left(Invalid(path))
    }
}
