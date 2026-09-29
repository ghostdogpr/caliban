package caliban.gateway.internal.acquisition

import caliban.{ GraphQLRequest, InputValue }
import caliban.gateway.{ RemoteGraphQLConfig, SchemaAcquisitionError, SubgraphAcquisitionError }
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
import caliban.Value.StringValue
import zio.{ IO, Trace, ZIO }
import zio.http.URL

private[gateway] object IntrospectionClient {

  def fetch(
    endpoint: URL,
    config: RemoteGraphQLConfig.Acquisition,
    http: GatewayHttpClient
  )(implicit trace: Trace): IO[SubgraphAcquisitionError, Document] =
    fetchData[SubgraphAcquisitionError](endpoint, Request, config, http, RedirectScope.AnyOrigin)(
      !_.isRedirection,
      IntrospectionErrors(_)
    )
      .flatMap(data => ZIO.fromEither(decode(data, config.maxParsingDepth)))

  private def decode(data: JsonObject, maxDepth: Int): Either[SchemaAcquisitionError, Document] =
    for {
      schema           <- data.obj("__schema")
      queryType        <- schema("queryType").optional(namedType)
      mutationType     <- schema("mutationType").optional(namedType)
      subscriptionType <- schema("subscriptionType").optional(namedType)
      types            <- schema("types").list(typeDefinition(maxDepth))
      directives       <- schema("directives").list(directive(maxDepth))
    } yield {
      val definition =
        SchemaDefinition(Nil, queryType.map(_.name), mutationType.map(_.name), subscriptionType.map(_.name), None)
      val userTypes  = types.filterNot(_.name.startsWith("__"))
      Document(definition :: userTypes ++ directives, SourceMapper.empty)
    }

  private def typeDefinition(maxDepth: Int)(json: Json): Either[SchemaAcquisitionError, TypeDefinition] =
    for {
      obj           <- json.obj
      kind          <- obj.string("kind")
      name          <- obj.string("name")
      description   <- obj("description").optional(_.string)
      fields        <- obj("fields").listOrNil(field(maxDepth))
      inputFields   <- obj("inputFields").listOrNil(inputValue(maxDepth))
      interfaces    <- obj("interfaces").listOrNil(namedType)
      enumValues    <- obj("enumValues").listOrNil(enumValue)
      possibleTypes <- obj("possibleTypes").listOrNil(namedType)
      definition    <- kind match {
                         case "SCALAR"       => Right(ScalarTypeDefinition(description, name, Nil))
                         case "OBJECT"       => Right(ObjectTypeDefinition(description, name, interfaces, Nil, fields))
                         case "INTERFACE"    => Right(InterfaceTypeDefinition(description, name, interfaces, Nil, fields))
                         case "UNION"        => Right(UnionTypeDefinition(description, name, Nil, possibleTypes.map(_.name)))
                         case "ENUM"         => Right(EnumTypeDefinition(description, name, Nil, enumValues))
                         case "INPUT_OBJECT" => Right(InputObjectTypeDefinition(description, name, Nil, inputFields))
                         case _              => Left(obj("kind").invalid)
                       }
    } yield definition

  private def field(maxDepth: Int)(json: Json): Either[SchemaAcquisitionError, FieldDefinition] =
    for {
      obj         <- json.obj
      name        <- obj.string("name")
      description <- obj("description").optional(_.string)
      args        <- obj("args").listOrNil(inputValue(maxDepth))
      tpe         <- typeRef(obj("type"))
      deprecation <- deprecationDirectives(obj)
    } yield FieldDefinition(description, name, args, tpe, deprecation)

  private def inputValue(maxDepth: Int)(json: Json): Either[SchemaAcquisitionError, InputValueDefinition] =
    for {
      obj          <- json.obj
      name         <- obj.string("name")
      description  <- obj("description").optional(_.string)
      tpe          <- typeRef(obj("type"))
      defaultValue <-
        obj("defaultValue").optional(_.string.flatMap(parseWithinDepth(_, maxDepth)(Parser.parseInputValue)))
      deprecation  <- deprecationDirectives(obj)
    } yield InputValueDefinition(description, name, tpe, defaultValue, deprecation)

  private def enumValue(json: Json): Either[InvalidResponse, EnumValueDefinition] =
    for {
      obj         <- json.obj
      name        <- obj.string("name")
      description <- obj("description").optional(_.string)
      deprecation <- deprecationDirectives(obj)
    } yield EnumValueDefinition(description, name, deprecation)

  private def directive(maxDepth: Int)(json: Json): Either[SchemaAcquisitionError, DirectiveDefinition] =
    for {
      obj         <- json.obj
      name        <- obj.string("name")
      description <- obj("description").optional(_.string)
      locations   <- obj("locations").list(directiveLocation)
      args        <- obj("args").listOrNil(inputValue(maxDepth))
      repeatable  <- obj("isRepeatable").booleanOrFalse
    } yield DirectiveDefinition(description, name, args, repeatable, locations.toSet)

  private def typeRef(json: Json): Either[InvalidResponse, Type] =
    for {
      obj  <- json.obj
      kind <- obj.string("kind")
      tpe  <- kind match {
                case "NON_NULL" =>
                  val ofType = obj("ofType")
                  typeRef(ofType).filterOrElse(_.nullable, InvalidResponse(s"${ofType.path}.kind")).map(_.toNonNullable)
                case "LIST"     => typeRef(obj("ofType")).map(ListType(_, nonNull = false))
                case _          => obj.string("name").map(NamedType(_, nonNull = false))
              }
    } yield tpe

  private def namedType(json: Json): Either[InvalidResponse, NamedType] =
    json.obj.flatMap(_.string("name")).map(NamedType(_, nonNull = false))

  private def deprecationDirectives(obj: JsonObject): Either[InvalidResponse, List[Directive]] =
    for {
      deprecated <- obj("isDeprecated").booleanOrFalse
      reason     <- obj("deprecationReason").optional(_.string)
    } yield
      if (!deprecated) Nil
      else
        List(
          Directive(
            Directives.DeprecatedDirective,
            reason.fold(Map.empty[String, InputValue])(value => Map("reason" -> StringValue(value)))
          )
        )

  private def directiveLocation(json: Json): Either[InvalidResponse, DirectiveLocation] =
    json.string.flatMap { name =>
      fastparse.parse(name, Parsers.directiveLocation(_)) match {
        case fastparse.Parsed.Success(location, index) if index == name.length => Right(location)
        case _                                                                 => Left(json.invalid)
      }
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
