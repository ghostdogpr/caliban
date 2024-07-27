package caliban.tools

import caliban.InputValue
import caliban.Value.StringValue
import caliban.client.IntrospectionClient._
import caliban.client.Operations.RootQuery
import caliban.client.{ CalibanClientError, SelectionBuilder }
import caliban.parsing.Parser
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveLocation._
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.{ DirectiveDefinition, SchemaDefinition, TypeDefinition }
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.{ Directive, Document, Type }
import sttp.client3._
import sttp.model.Uri
import zio.{ RIO, ZIO }

object IntrospectionClient {

  def introspect(
    uri: String,
    headers: Option[List[Options.Header]]
  ): RIO[SttpClient, Document] =
    introspect(uri, headers, Config.default)

  @deprecated("Use overloaded method that accepts a config instead", "2.8.2")
  def introspect(
    uri: String,
    headers: Option[List[Options.Header]],
    supportIsRepeatable: Boolean = true
  ): RIO[SttpClient, Document] =
    introspect(uri, headers, Config.default.supportIsRepeatable(supportIsRepeatable))

  def introspect(
    uri: String,
    headers: Option[List[Options.Header]],
    config: IntrospectionClient.Config
  ): RIO[SttpClient, Document] =
    for {
      parsedUri <- ZIO.fromEither(Uri.parse(uri)).mapError(cause => new Exception(s"Invalid URL: $cause"))
      baseReq    = introspection(config).toRequest(parsedUri, dropNullInputValues = true)
      req        = headers.map(_.map(h => h.name -> h.value).toMap).fold(baseReq)(baseReq.headers)
      result    <- sendRequest(req)
    } yield result

  private def sendRequest[T](req: Request[Either[CalibanClientError, T], Any]): RIO[SttpClient, T] =
    ZIO.serviceWithZIO[SttpClient](_.send(req)).map(_.body).absolve

  private def directives(isDeprecated: Boolean, deprecationReason: Option[String]): List[Directive] =
    if (isDeprecated)
      List(
        Directive(
          "deprecated",
          deprecationReason.fold(Map.empty[String, InputValue])(reason => Map("reason" -> StringValue(reason)))
        )
      )
    else Nil

  private def mapEnumValue(
    name: String,
    description: Option[String],
    isDeprecated: Boolean,
    deprecationReason: Option[String]
  ): EnumValueDefinition =
    EnumValueDefinition(description, name, directives(isDeprecated, deprecationReason))

  private def mapInputValue(
    name: String,
    description: Option[String],
    `type`: Type,
    defaultValue: Option[String],
    isDeprecated: Boolean,
    deprecationReason: Option[String]
  ): InputValueDefinition = {
    val default = defaultValue.flatMap(v => Parser.parseInputValue(v).toOption)
    InputValueDefinition(description, name, `type`, default, directives(isDeprecated, deprecationReason))
  }

  private def mapTypeRef(kind: __TypeKind, name: Option[String], of: Option[Type]): Type =
    of match {
      case Some(value) =>
        kind match {
          case __TypeKind.LIST     => ListType(value, nonNull = false)
          case __TypeKind.NON_NULL =>
            value match {
              case NamedType(name, _)  => NamedType(name, nonNull = true)
              case ListType(ofType, _) => ListType(ofType, nonNull = true)
            }
          case _                   => NamedType(name.getOrElse(""), nonNull = false)
        }
      case None        => NamedType(name.getOrElse(""), nonNull = false)
    }

  private def mapTypeRefSimple(name: Option[String]): Type =
    NamedType(name.getOrElse(""), nonNull = true)

  private def mapField(
    name: String,
    description: Option[String],
    args: List[InputValueDefinition],
    `type`: Type,
    isDeprecated: Boolean,
    deprecationReason: Option[String]
  ): FieldDefinition = FieldDefinition(description, name, args, `type`, directives(isDeprecated, deprecationReason))

  private def mapType(
    kind: __TypeKind,
    name: Option[String],
    description: Option[String],
    fields: Option[List[FieldDefinition]],
    inputFields: Option[List[InputValueDefinition]],
    interfaces: Option[List[Type]],
    enumValues: Option[List[EnumValueDefinition]],
    possibleTypes: Option[List[Type]]
  ): Option[TypeDefinition] = kind match {
    case __TypeKind.SCALAR                     =>
      Some(ScalarTypeDefinition(description, name.getOrElse(""), Nil))
    case __TypeKind.OBJECT                     =>
      Some(
        ObjectTypeDefinition(
          description,
          name.getOrElse(""),
          interfaces.map(_.collect { case t: NamedType => t }).getOrElse(Nil),
          Nil,
          fields.getOrElse(Nil)
        )
      )
    case __TypeKind.INTERFACE                  =>
      Some(
        InterfaceTypeDefinition(
          description,
          name.getOrElse(""),
          interfaces.map(_.collect { case t: NamedType => t }).getOrElse(Nil),
          Nil,
          fields.getOrElse(Nil)
        )
      )
    case __TypeKind.UNION                      =>
      Some(
        UnionTypeDefinition(
          description,
          name.getOrElse(""),
          Nil,
          possibleTypes.getOrElse(Nil).collect { case NamedType(name, _) =>
            name
          }
        )
      )
    case __TypeKind.ENUM                       =>
      Some(EnumTypeDefinition(description, name.getOrElse(""), Nil, enumValues.getOrElse(Nil)))
    case __TypeKind.INPUT_OBJECT               =>
      Some(InputObjectTypeDefinition(description, name.getOrElse(""), Nil, inputFields.getOrElse(Nil)))
    case __TypeKind.LIST | __TypeKind.NON_NULL => None
  }

  private def mapSchema(
    query: Option[String],
    mutation: Option[Option[String]],
    subscription: Option[Option[String]]
  ): SchemaDefinition =
    SchemaDefinition(Nil, query, mutation.flatten, subscription.flatten, None)

  private def mapDirective(
    name: String,
    description: Option[String],
    locations: List[__DirectiveLocation],
    args: List[InputValueDefinition],
    isRepeatable: Boolean
  ): DirectiveDefinition =
    DirectiveDefinition(
      description,
      name,
      args,
      isRepeatable,
      locations.map {
        case __DirectiveLocation.QUERY                  => ExecutableDirectiveLocation.QUERY
        case __DirectiveLocation.MUTATION               => ExecutableDirectiveLocation.MUTATION
        case __DirectiveLocation.SUBSCRIPTION           => ExecutableDirectiveLocation.SUBSCRIPTION
        case __DirectiveLocation.FIELD                  => ExecutableDirectiveLocation.FIELD
        case __DirectiveLocation.FRAGMENT_DEFINITION    => ExecutableDirectiveLocation.FRAGMENT_DEFINITION
        case __DirectiveLocation.FRAGMENT_SPREAD        => ExecutableDirectiveLocation.FRAGMENT_SPREAD
        case __DirectiveLocation.INLINE_FRAGMENT        => ExecutableDirectiveLocation.INLINE_FRAGMENT
        case __DirectiveLocation.SCHEMA                 => TypeSystemDirectiveLocation.SCHEMA
        case __DirectiveLocation.SCALAR                 => TypeSystemDirectiveLocation.SCALAR
        case __DirectiveLocation.OBJECT                 => TypeSystemDirectiveLocation.OBJECT
        case __DirectiveLocation.FIELD_DEFINITION       => TypeSystemDirectiveLocation.FIELD_DEFINITION
        case __DirectiveLocation.ARGUMENT_DEFINITION    => TypeSystemDirectiveLocation.ARGUMENT_DEFINITION
        case __DirectiveLocation.INTERFACE              => TypeSystemDirectiveLocation.INTERFACE
        case __DirectiveLocation.UNION                  => TypeSystemDirectiveLocation.UNION
        case __DirectiveLocation.ENUM                   => TypeSystemDirectiveLocation.ENUM
        case __DirectiveLocation.ENUM_VALUE             => TypeSystemDirectiveLocation.ENUM_VALUE
        case __DirectiveLocation.INPUT_OBJECT           => TypeSystemDirectiveLocation.INPUT_OBJECT
        case __DirectiveLocation.INPUT_FIELD_DEFINITION => TypeSystemDirectiveLocation.INPUT_FIELD_DEFINITION
        case __DirectiveLocation.VARIABLE_DEFINITION    => TypeSystemDirectiveLocation.VARIABLE_DEFINITION
      }.toSet
    )

  private val typeRef: SelectionBuilder[__Type, Type] =
    (__Type.kind ~ __Type.name ~ __Type.ofType {
      (__Type.kind ~ __Type.name ~ __Type.ofType {
        (__Type.kind ~ __Type.name ~ __Type.ofType {
          (__Type.kind ~ __Type.name ~ __Type.ofType {
            (__Type.kind ~ __Type.name ~ __Type.ofType {
              (__Type.kind ~ __Type.name ~ __Type.ofType {
                (__Type.kind ~ __Type.name ~ __Type.ofType {
                  (__Type.kind ~ __Type.name ~ __Type.ofType {
                    __Type.name.map(mapTypeRefSimple)
                  }).mapN(mapTypeRef _)
                }).mapN(mapTypeRef _)
              }).mapN(mapTypeRef _)
            }).mapN(mapTypeRef _)
          }).mapN(mapTypeRef _)
        }).mapN(mapTypeRef _)
      }).mapN(mapTypeRef _)
    }).mapN(mapTypeRef _)

  private def inputValue(implicit
    config: Config
  ): SelectionBuilder[__InputValue, InputValueDefinition] =
    if (config.supportDeprecatedArgs)
      (
        __InputValue.name ~
          __InputValue.description ~
          __InputValue.`type`(typeRef) ~
          __InputValue.defaultValue ~
          __InputValue.isDeprecated ~
          __InputValue.deprecationReason
      ).mapN(mapInputValue _)
    else
      (
        __InputValue.name ~
          __InputValue.description ~
          __InputValue.`type`(typeRef) ~
          __InputValue.defaultValue
      ).mapN(mapInputValue(_, _, _, _, isDeprecated = false, None))

  private def fullType(implicit config: Config): SelectionBuilder[__Type, Option[TypeDefinition]] =
    (__Type.kind ~
      __Type.name ~
      __Type.description ~
      __Type.fields(Some(true)) {
        (__Field.name ~
          __Field.description ~
          __Field.args(if (config.supportDeprecatedArgs) Some(true) else None)(inputValue) ~
          __Field.`type`(typeRef) ~
          __Field.isDeprecated ~
          __Field.deprecationReason).mapN(mapField _)
      } ~
      __Type.inputFields(if (config.supportDeprecatedArgs) Some(true) else None)(inputValue) ~
      __Type.interfaces(typeRef) ~
      __Type.enumValues(Some(true)) {
        (__EnumValue.name ~
          __EnumValue.description ~
          __EnumValue.isDeprecated ~
          __EnumValue.deprecationReason).mapN(mapEnumValue _)
      } ~
      __Type.possibleTypes(typeRef)).mapN(mapType _)

  @deprecated("Use overloaded method that accepts a list of experimental features", "2.8.2")
  def introspection(supportIsRepeatable: Boolean): SelectionBuilder[RootQuery, Document] = {
    val cfg = Config.default.supportIsRepeatable(supportIsRepeatable)
    introspection(cfg)
  }

  def introspection(implicit config: Config): SelectionBuilder[RootQuery, Document] =
    Query.__schema {
      (__Schema.queryType(__Type.name) ~
        __Schema.mutationType(__Type.name) ~
        __Schema.subscriptionType(__Type.name)).mapN(mapSchema _) ~
        __Schema.types(fullType).map(_.flatten.filterNot(_.name.startsWith("__"))) ~
        __Schema.directives {
          if (config.supportIsRepeatable)
            (__Directive.name ~
              __Directive.description ~
              __Directive.locations ~
              __Directive.args(inputValue) ~
              __Directive.isRepeatable).mapN(mapDirective _)
          else
            (__Directive.name ~
              __Directive.description ~
              __Directive.locations ~
              __Directive.args(inputValue)).mapN(mapDirective(_, _, _, _, isRepeatable = false))
        }
    }.map { case (schema, types, directives) => Document(schema :: types ++ directives, SourceMapper.empty) }

  final class Config(
    val supportDeprecatedArgs: Boolean = true,
    val supportIsRepeatable: Boolean = true
  ) {

    def supportDeprecatedArgs(value: Boolean): Config =
      new Config(supportDeprecatedArgs = value, supportIsRepeatable = supportIsRepeatable)

    def supportIsRepeatable(value: Boolean): Config =
      new Config(supportDeprecatedArgs = supportDeprecatedArgs, supportIsRepeatable = value)

    override def toString: String =
      s"Config(supportDeprecatedArgs = $supportDeprecatedArgs, supportIsRepeatable = $supportIsRepeatable)"
  }

  object Config {
    val default: Config = new Config()

    def apply(supportDeprecatedArgs: Boolean, supportIsRepeatable: Boolean): Config =
      new Config(supportDeprecatedArgs, supportIsRepeatable)
  }
}
