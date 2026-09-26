package caliban.gateway.internal.acquisition

import caliban.{ GraphQLRequest, InputValue, ResponseValue }
import caliban.gateway.{ traverseEither, RemoteGraphQLConfig, SchemaAcquisitionError, SubgraphAcquisitionError }
import caliban.gateway.SchemaAcquisitionError.InvalidResponse
import caliban.gateway.SubgraphAcquisitionError.IntrospectionErrors
import caliban.gateway.internal.GatewayHttpClient
import caliban.gateway.internal.acquisition.RemoteSchemaAcquisition._
import caliban.parsing.adt.{ Directive, Directives, Document, Type }
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.parsers.Parsers
import caliban.parsing.Parser
import caliban.parsing.SourceMapper
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ BooleanValue, NullValue, StringValue }
import zio.{ IO, Trace, ZIO }
import zio.http.URL

private[gateway] object IntrospectionClient {

  def fetch(
    endpoint: URL,
    config: RemoteGraphQLConfig.Acquisition,
    http: GatewayHttpClient
  )(implicit trace: Trace): IO[SubgraphAcquisitionError, Document] =
    fetchData[SubgraphAcquisitionError](endpoint, Request, config, http)(!_.isRedirection, IntrospectionErrors(_))
      .flatMap(data => ZIO.fromEither(decode(data, config.maxParsingDepth)))

  private def decode(data: ObjectValue, maxDepth: Int): Either[SchemaAcquisitionError, Document] =
    for {
      schema           <- objectField(data, "__schema", "$.data")
      queryType        <- optional(schema, "queryType", "$.data.__schema")(namedType)
      mutationType     <- optional(schema, "mutationType", "$.data.__schema")(namedType)
      subscriptionType <- optional(schema, "subscriptionType", "$.data.__schema")(namedType)
      types            <- list(schema, "types", "$.data.__schema")(typeDefinition(maxDepth))
      directives       <- list(schema, "directives", "$.data.__schema")(directive(maxDepth))
    } yield {
      val definition =
        SchemaDefinition(Nil, queryType.map(_.name), mutationType.map(_.name), subscriptionType.map(_.name), None)
      val userTypes  = types.filterNot(_.name.startsWith("__"))
      Document(definition :: userTypes ++ directives, SourceMapper.empty)
    }

  private def typeDefinition(
    maxDepth: Int
  )(value: ResponseValue, path: String): Either[SchemaAcquisitionError, TypeDefinition] =
    for {
      obj           <- asObject(value, path)
      kind          <- string(obj, "kind", path)
      name          <- string(obj, "name", path)
      description   <- optional(obj, "description", path)(asString)
      fields        <- listOrNil(obj, "fields", path)(field(maxDepth))
      inputFields   <- listOrNil(obj, "inputFields", path)(inputValue(maxDepth))
      interfaces    <- listOrNil(obj, "interfaces", path)(namedType)
      enumValues    <- listOrNil(obj, "enumValues", path)(enumValue)
      possibleTypes <- listOrNil(obj, "possibleTypes", path)(namedType)
      definition    <- kind match {
                         case "SCALAR"       => Right(ScalarTypeDefinition(description, name, Nil))
                         case "OBJECT"       => Right(ObjectTypeDefinition(description, name, interfaces, Nil, fields))
                         case "INTERFACE"    => Right(InterfaceTypeDefinition(description, name, interfaces, Nil, fields))
                         case "UNION"        => Right(UnionTypeDefinition(description, name, Nil, possibleTypes.map(_.name)))
                         case "ENUM"         => Right(EnumTypeDefinition(description, name, Nil, enumValues))
                         case "INPUT_OBJECT" => Right(InputObjectTypeDefinition(description, name, Nil, inputFields))
                         case _              => Left(InvalidResponse(s"$path.kind"))
                       }
    } yield definition

  private def field(
    maxDepth: Int
  )(value: ResponseValue, path: String): Either[SchemaAcquisitionError, FieldDefinition] =
    for {
      obj         <- asObject(value, path)
      name        <- string(obj, "name", path)
      description <- optional(obj, "description", path)(asString)
      args        <- listOrNil(obj, "args", path)(inputValue(maxDepth))
      tpe         <- typeRef(obj.getOrNull("type"), s"$path.type")
      deprecation <- deprecationDirectives(obj, path)
    } yield FieldDefinition(description, name, args, tpe, deprecation)

  private def inputValue(
    maxDepth: Int
  )(value: ResponseValue, path: String): Either[SchemaAcquisitionError, InputValueDefinition] =
    for {
      obj          <- asObject(value, path)
      name         <- string(obj, "name", path)
      description  <- optional(obj, "description", path)(asString)
      tpe          <- typeRef(obj.getOrNull("type"), s"$path.type")
      defaultValue <- optional(obj, "defaultValue", path)((raw, rawPath) =>
                        asString(raw, rawPath).flatMap(parseWithinDepth(_, maxDepth)(Parser.parseInputValue))
                      )
      deprecation  <- deprecationDirectives(obj, path)
    } yield InputValueDefinition(description, name, tpe, defaultValue, deprecation)

  private def enumValue(value: ResponseValue, path: String): Either[InvalidResponse, EnumValueDefinition] =
    for {
      obj         <- asObject(value, path)
      name        <- string(obj, "name", path)
      description <- optional(obj, "description", path)(asString)
      deprecation <- deprecationDirectives(obj, path)
    } yield EnumValueDefinition(description, name, deprecation)

  private def directive(
    maxDepth: Int
  )(value: ResponseValue, path: String): Either[SchemaAcquisitionError, DirectiveDefinition] =
    for {
      obj         <- asObject(value, path)
      name        <- string(obj, "name", path)
      description <- optional(obj, "description", path)(asString)
      locations   <- list(obj, "locations", path)(directiveLocation)
      args        <- listOrNil(obj, "args", path)(inputValue(maxDepth))
      repeatable  <- booleanOrFalse(obj, "isRepeatable", path)
    } yield DirectiveDefinition(description, name, args, repeatable, locations.toSet)

  private def typeRef(value: ResponseValue, path: String): Either[InvalidResponse, Type] =
    for {
      obj  <- asObject(value, path)
      kind <- string(obj, "kind", path)
      tpe  <- kind match {
                case "NON_NULL" =>
                  typeRef(obj.getOrNull("ofType"), s"$path.ofType")
                    .filterOrElse(_.nullable, InvalidResponse(s"$path.ofType.kind"))
                    .map(_.toNonNullable)
                case "LIST"     => typeRef(obj.getOrNull("ofType"), s"$path.ofType").map(ListType(_, nonNull = false))
                case _          => namedType(obj, path)
              }
    } yield tpe

  private def namedType(value: ResponseValue, path: String): Either[InvalidResponse, NamedType] =
    asObject(value, path).flatMap(string(_, "name", path)).map(NamedType(_, nonNull = false))

  private def deprecationDirectives(obj: ObjectValue, path: String): Either[InvalidResponse, List[Directive]] =
    for {
      deprecated <- booleanOrFalse(obj, "isDeprecated", path)
      reason     <- optional(obj, "deprecationReason", path)(asString)
    } yield
      if (!deprecated) Nil
      else
        List(
          Directive(
            Directives.DeprecatedDirective,
            reason.fold(Map.empty[String, InputValue])(value => Map("reason" -> StringValue(value)))
          )
        )

  private def directiveLocation(value: ResponseValue, path: String): Either[InvalidResponse, DirectiveLocation] =
    asString(value, path).flatMap { name =>
      fastparse.parse(name, Parsers.directiveLocation(_)) match {
        case fastparse.Parsed.Success(location, index) if index == name.length => Right(location)
        case _                                                                 => Left(InvalidResponse(path))
      }
    }

  private def optional[E, A](obj: ObjectValue, field: String, path: String)(
    read: (ResponseValue, String) => Either[E, A]
  ): Either[E, Option[A]] =
    obj.getOrNull(field) match {
      case null | NullValue => Right(None)
      case value            => read(value, s"$path.$field").map(Some(_))
    }

  private def booleanOrFalse(obj: ObjectValue, field: String, path: String): Either[InvalidResponse, Boolean] =
    obj.getOrNull(field) match {
      case null | NullValue    => Right(false)
      case BooleanValue(value) => Right(value)
      case _                   => Left(InvalidResponse(s"$path.$field"))
    }

  private def list[A](obj: ObjectValue, field: String, path: String)(
    read: (ResponseValue, String) => Either[SchemaAcquisitionError, A]
  ): Either[SchemaAcquisitionError, List[A]] =
    obj.getOrNull(field) match {
      case ListValue(items) =>
        traverseEither(items.zipWithIndex) { case (item, index) => read(item, s"$path.$field[$index]") }
      case _                => Left(InvalidResponse(s"$path.$field"))
    }

  private def listOrNil[A](obj: ObjectValue, field: String, path: String)(
    read: (ResponseValue, String) => Either[SchemaAcquisitionError, A]
  ): Either[SchemaAcquisitionError, List[A]] =
    obj.getOrNull(field) match {
      case null | NullValue => Right(Nil)
      case _                => list(obj, field, path)(read)
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

  private val Request = GraphQLRequest(query = Some(Query), operationName = Some(OperationName))
}
