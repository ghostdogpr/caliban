package caliban.gateway.internal.composition

import caliban.{ InputValue, ResponseValue }
import caliban.gateway.{ traverseEither, RemoteGraphQLConfig, SubgraphAcquisitionError }
import caliban.gateway.SubgraphAcquisitionError._
import caliban.gateway.internal.GatewayHttpClient
import caliban.parsing.adt.{ Directive, Directives, Document, Type }
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveLocation._
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.Parser
import caliban.parsing.SourceMapper
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ BooleanValue, NullValue, StringValue }

import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import zio.{ IO, Trace, ZIO }
import zio.http.URL

private[gateway] object IntrospectionClient {

  def fetch(
    endpoint: URL,
    config: RemoteGraphQLConfig.Acquisition,
    http: GatewayHttpClient
  )(implicit trace: Trace): IO[SubgraphAcquisitionError, Document] =
    for {
      bytes    <- RemoteSchemaAcquisition.fetchBytes(endpoint, Query, OperationName, config, http)
      response <- ZIO.attempt(readFromArray[ResponseValue](bytes)).mapError(IntrospectionResponseDecodingFailed(_))
      _        <- ZIO
                    .fail(ParsingDepthExceeded(config.maxParsingDepth))
                    .unless(defaultValuesWithinDepth(response, config.maxParsingDepth))
      envelope <- ZIO.fromEither(asObject(response, "response")).mapError(IntrospectionResponseDecodingFailed(_))
      errors   <- ZIO
                    .fromOption(RemoteSchemaAcquisition.responseErrors(envelope))
                    .orElseFail(IntrospectionResponseDecodingFailed(InvalidResponse("errors")))
      _        <- ZIO.fail(IntrospectionErrors(errors)).when(errors.nonEmpty)
      document <- ZIO.fromEither(decode(envelope.getOrNull("data"))).mapError(IntrospectionResponseDecodingFailed(_))
    } yield document

  private final case class InvalidResponse(path: String)
      extends Exception(s"Unexpected introspection response shape at '$path'.")

  private def decode(data: ResponseValue): Either[InvalidResponse, Document] =
    for {
      schema           <- objectField(data, "__schema", "__schema")
      queryType        <- rootTypeName(schema, "queryType")
      mutationType     <- rootTypeName(schema, "mutationType")
      subscriptionType <- rootTypeName(schema, "subscriptionType")
      types            <- list(schema, "types", "__schema.types")(typeDefinition)
      directives       <- list(schema, "directives", "__schema.directives")(directive)
    } yield {
      val definition = SchemaDefinition(Nil, queryType, mutationType, subscriptionType, None)
      val userTypes  = types.flatten.filterNot(_.name.startsWith("__"))
      Document(definition :: userTypes ++ directives, SourceMapper.empty)
    }

  private def rootTypeName(schema: ObjectValue, field: String): Either[InvalidResponse, Option[String]] =
    schema.getOrNull(field) match {
      case null | NullValue   => Right(None)
      case value: ObjectValue => optionalString(value, "name", s"__schema.$field.name")
      case _                  => Left(InvalidResponse(s"__schema.$field"))
    }

  private def typeDefinition(value: ResponseValue, path: String): Either[InvalidResponse, Option[TypeDefinition]] =
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
                           Right(
                             Some(UnionTypeDefinition(description, name, Nil, namedTypes(possibleTypes).map(_.name)))
                           )
                         case "ENUM"              => Right(Some(EnumTypeDefinition(description, name, Nil, enumValues)))
                         case "INPUT_OBJECT"      =>
                           Right(Some(InputObjectTypeDefinition(description, name, Nil, inputFields)))
                         case "LIST" | "NON_NULL" => Right(None)
                         case _                   => Left(InvalidResponse(s"$path.kind"))
                       }
    } yield definition

  private def namedTypes(types: List[Type]): List[NamedType] = types.collect { case named: NamedType => named }

  private def field(value: ResponseValue, path: String): Either[InvalidResponse, FieldDefinition] =
    for {
      obj         <- asObject(value, path)
      name        <- string(obj, "name", path)
      description <- optionalString(obj, "description", path)
      args        <- optionalList(obj, "args", s"$path.args")(inputValue).map(_.getOrElse(Nil))
      tpe         <- typeRef(obj.getOrNull("type"), s"$path.type")
      deprecation <- deprecationDirectives(obj, path)
    } yield FieldDefinition(description, name, args, tpe, deprecation)

  private def inputValue(value: ResponseValue, path: String): Either[InvalidResponse, InputValueDefinition] =
    for {
      obj         <- asObject(value, path)
      name        <- string(obj, "name", path)
      description <- optionalString(obj, "description", path)
      tpe         <- typeRef(obj.getOrNull("type"), s"$path.type")
      default     <- optionalString(obj, "defaultValue", path)
      deprecation <- deprecationDirectives(obj, path)
    } yield InputValueDefinition(
      description,
      name,
      tpe,
      default.flatMap(raw => Parser.parseInputValue(raw).toOption),
      deprecation
    )

  private def enumValue(value: ResponseValue, path: String): Either[InvalidResponse, EnumValueDefinition] =
    for {
      obj         <- asObject(value, path)
      name        <- string(obj, "name", path)
      description <- optionalString(obj, "description", path)
      deprecation <- deprecationDirectives(obj, path)
    } yield EnumValueDefinition(description, name, deprecation)

  private def directive(value: ResponseValue, path: String): Either[InvalidResponse, DirectiveDefinition] =
    for {
      obj         <- asObject(value, path)
      name        <- string(obj, "name", path)
      description <- optionalString(obj, "description", path)
      locations   <- list(obj, "locations", s"$path.locations")((location, locationPath) =>
                       location match {
                         case StringValue(name) => directiveLocation(name).toRight(InvalidResponse(locationPath))
                         case _                 => Left(InvalidResponse(locationPath))
                       }
                     )
      args        <- optionalList(obj, "args", s"$path.args")(inputValue).map(_.getOrElse(Nil))
      repeatable  <- booleanOrFalse(obj, "isRepeatable", path)
    } yield DirectiveDefinition(description, name, args, repeatable, locations.toSet)

  private def typeRef(value: ResponseValue, path: String): Either[InvalidResponse, Type] =
    for {
      obj  <- asObject(value, path)
      kind <- string(obj, "kind", path)
      tpe  <- kind match {
                case "NON_NULL" =>
                  wrappedType(obj, path).map {
                    case NamedType(name, _)  => NamedType(name, nonNull = true)
                    case ListType(ofType, _) => ListType(ofType, nonNull = true)
                  }
                case "LIST"     => wrappedType(obj, path).map(ListType(_, nonNull = false))
                case _          =>
                  optionalString(obj, "name", path).map(name => NamedType(name.getOrElse(""), nonNull = false))
              }
    } yield tpe

  private def wrappedType(obj: ObjectValue, path: String): Either[InvalidResponse, Type] =
    obj.getOrNull("ofType") match {
      case null | NullValue => Right(NamedType("", nonNull = false))
      case value            => typeRef(value, s"$path.ofType")
    }

  private def deprecationDirectives(obj: ObjectValue, path: String): Either[InvalidResponse, List[Directive]] =
    for {
      deprecated <- booleanOrFalse(obj, "isDeprecated", path)
      reason     <- optionalString(obj, "deprecationReason", path)
    } yield
      if (!deprecated) Nil
      else
        List(
          Directive(
            Directives.DeprecatedDirective,
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

  private def asObject(value: ResponseValue, path: String): Either[InvalidResponse, ObjectValue] =
    value match {
      case obj: ObjectValue => Right(obj)
      case _                => Left(InvalidResponse(path))
    }

  private def objectField(value: ResponseValue, field: String, path: String): Either[InvalidResponse, ObjectValue] =
    asObject(value, path).flatMap(obj => asObject(obj.getOrNull(field), path))

  private def string(obj: ObjectValue, field: String, path: String): Either[InvalidResponse, String] =
    obj.getOrNull(field) match {
      case StringValue(value) => Right(value)
      case _                  => Left(InvalidResponse(s"$path.$field"))
    }

  private def optionalString(obj: ObjectValue, field: String, path: String): Either[InvalidResponse, Option[String]] =
    obj.getOrNull(field) match {
      case null | NullValue   => Right(None)
      case StringValue(value) => Right(Some(value))
      case _                  => Left(InvalidResponse(s"$path.$field"))
    }

  private def booleanOrFalse(obj: ObjectValue, field: String, path: String): Either[InvalidResponse, Boolean] =
    obj.getOrNull(field) match {
      case null | NullValue    => Right(false)
      case BooleanValue(value) => Right(value)
      case _                   => Left(InvalidResponse(s"$path.$field"))
    }

  private def list[A](obj: ObjectValue, field: String, path: String)(
    read: (ResponseValue, String) => Either[InvalidResponse, A]
  ): Either[InvalidResponse, List[A]] =
    optionalList(obj, field, path)(read).flatMap(_.toRight(InvalidResponse(path)))

  private def optionalList[A](obj: ObjectValue, field: String, path: String)(
    read: (ResponseValue, String) => Either[InvalidResponse, A]
  ): Either[InvalidResponse, Option[List[A]]] =
    obj.getOrNull(field) match {
      case null | NullValue => Right(None)
      case ListValue(items) =>
        traverseEither(items.zipWithIndex) { case (item, index) => read(item, s"$path[$index]") }.map(Some(_))
      case _                => Left(InvalidResponse(path))
    }

  private def defaultValuesWithinDepth(value: ResponseValue, maxDepth: Int): Boolean =
    value match {
      case ObjectValue(fields) =>
        fields.forall {
          case ("defaultValue", StringValue(defaultValue)) =>
            RemoteSchemaAcquisition.withinGraphQLDepth(defaultValue, maxDepth)
          case (_, nested)                                 => defaultValuesWithinDepth(nested, maxDepth)
        }
      case ListValue(values)   => values.forall(defaultValuesWithinDepth(_, maxDepth))
      case _                   => true
    }

  private final val OperationName = "__CalibanGatewayIntrospection"

  // Shared with test fixtures that generate introspection responses.
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
}
