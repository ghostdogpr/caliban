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
    fetchData[SubgraphAcquisitionError](endpoint, Request, config, http)(isGraphQLResponse, IntrospectionErrors(_))
      .flatMap(data => ZIO.fromEither(decode(data, config.maxParsingDepth)))

  private def decode(data: ObjectValue, maxDepth: Int): Either[SchemaAcquisitionError, Document] =
    for {
      schema           <- objectField(data, "__schema", "$.data")
      queryType        <- rootTypeName(schema, "queryType")
      mutationType     <- rootTypeName(schema, "mutationType")
      subscriptionType <- rootTypeName(schema, "subscriptionType")
      types            <- list(schema, "types", "$.data.__schema.types")(typeDefinition(maxDepth))
      directives       <- list(schema, "directives", "$.data.__schema.directives")(directive(maxDepth))
    } yield {
      val definition = SchemaDefinition(Nil, queryType, mutationType, subscriptionType, None)
      val userTypes  = types.flatten.filterNot(_.name.startsWith("__"))
      Document(definition :: userTypes ++ directives, SourceMapper.empty)
    }

  private def rootTypeName(schema: ObjectValue, field: String): Either[InvalidResponse, Option[String]] =
    schema.getOrNull(field) match {
      case null | NullValue   => Right(None)
      case value: ObjectValue => optionalString(value, "name", s"$$.data.__schema.$field")
      case _                  => Left(InvalidResponse(s"$$.data.__schema.$field"))
    }

  private def typeDefinition(
    maxDepth: Int
  )(value: ResponseValue, path: String): Either[SchemaAcquisitionError, Option[TypeDefinition]] =
    for {
      obj           <- asObject(value, path)
      kind          <- string(obj, "kind", path)
      name          <- optionalString(obj, "name", path).map(_.getOrElse(""))
      description   <- optionalString(obj, "description", path)
      fields        <- optionalList(obj, "fields", s"$path.fields")(field(maxDepth)).map(_.getOrElse(Nil))
      inputFields   <-
        optionalList(obj, "inputFields", s"$path.inputFields")(inputValue(maxDepth)).map(_.getOrElse(Nil))
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

  private def field(
    maxDepth: Int
  )(value: ResponseValue, path: String): Either[SchemaAcquisitionError, FieldDefinition] =
    for {
      obj         <- asObject(value, path)
      name        <- string(obj, "name", path)
      description <- optionalString(obj, "description", path)
      args        <- optionalList(obj, "args", s"$path.args")(inputValue(maxDepth)).map(_.getOrElse(Nil))
      tpe         <- typeRef(obj.getOrNull("type"), s"$path.type")
      deprecation <- deprecationDirectives(obj, path)
    } yield FieldDefinition(description, name, args, tpe, deprecation)

  private def inputValue(
    maxDepth: Int
  )(value: ResponseValue, path: String): Either[SchemaAcquisitionError, InputValueDefinition] =
    for {
      obj          <- asObject(value, path)
      name         <- string(obj, "name", path)
      description  <- optionalString(obj, "description", path)
      tpe          <- typeRef(obj.getOrNull("type"), s"$path.type")
      default      <- optionalString(obj, "defaultValue", path)
      defaultValue <- default.fold[Either[SchemaAcquisitionError, Option[InputValue]]](Right(None))(raw =>
                        parseWithinDepth(raw, maxDepth)(value => Right(Parser.parseInputValue(value).toOption))
                      )
      deprecation  <- deprecationDirectives(obj, path)
    } yield InputValueDefinition(description, name, tpe, defaultValue, deprecation)

  private def enumValue(value: ResponseValue, path: String): Either[InvalidResponse, EnumValueDefinition] =
    for {
      obj         <- asObject(value, path)
      name        <- string(obj, "name", path)
      description <- optionalString(obj, "description", path)
      deprecation <- deprecationDirectives(obj, path)
    } yield EnumValueDefinition(description, name, deprecation)

  private def directive(
    maxDepth: Int
  )(value: ResponseValue, path: String): Either[SchemaAcquisitionError, DirectiveDefinition] =
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
      args        <- optionalList(obj, "args", s"$path.args")(inputValue(maxDepth)).map(_.getOrElse(Nil))
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
    fastparse.parse(name, Parsers.directiveLocation(_)) match {
      case fastparse.Parsed.Success(location, index) if index == name.length => Some(location)
      case _                                                                 => None
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
    read: (ResponseValue, String) => Either[SchemaAcquisitionError, A]
  ): Either[SchemaAcquisitionError, List[A]] =
    optionalList(obj, field, path)(read).flatMap(_.toRight(InvalidResponse(path)))

  private def optionalList[A](obj: ObjectValue, field: String, path: String)(
    read: (ResponseValue, String) => Either[SchemaAcquisitionError, A]
  ): Either[SchemaAcquisitionError, Option[List[A]]] =
    obj.getOrNull(field) match {
      case null | NullValue => Right(None)
      case ListValue(items) =>
        traverseEither(items.zipWithIndex) { case (item, index) => read(item, s"$path[$index]") }.map(Some(_))
      case _                => Left(InvalidResponse(path))
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
