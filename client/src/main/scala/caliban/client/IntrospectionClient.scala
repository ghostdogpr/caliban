package caliban.client

import caliban.client.CalibanClientError.DecodingError
import caliban.client.FieldBuilder.{ ListOf, Obj, OptionOf, Scalar }
import caliban.client.Operations.RootQuery
import caliban.client.SelectionBuilder.Field
import caliban.client.__Value.{ __EnumValue => EnumValue, __StringValue }

object IntrospectionClient {

  sealed trait __TypeKind extends scala.Product with scala.Serializable
  object __TypeKind {
    case object SCALAR       extends __TypeKind
    case object OBJECT       extends __TypeKind
    case object INTERFACE    extends __TypeKind
    case object UNION        extends __TypeKind
    case object ENUM         extends __TypeKind
    case object INPUT_OBJECT extends __TypeKind
    case object LIST         extends __TypeKind
    case object NON_NULL     extends __TypeKind

    implicit val decoder: ScalarDecoder[__TypeKind] = {
      case __StringValue("SCALAR")       => Right(__TypeKind.SCALAR)
      case __StringValue("OBJECT")       => Right(__TypeKind.OBJECT)
      case __StringValue("INTERFACE")    => Right(__TypeKind.INTERFACE)
      case __StringValue("UNION")        => Right(__TypeKind.UNION)
      case __StringValue("ENUM")         => Right(__TypeKind.ENUM)
      case __StringValue("INPUT_OBJECT") => Right(__TypeKind.INPUT_OBJECT)
      case __StringValue("LIST")         => Right(__TypeKind.LIST)
      case __StringValue("NON_NULL")     => Right(__TypeKind.NON_NULL)
      case other                         => Left(DecodingError(s"Can't build __TypeKind from input $other"))
    }
    implicit val encoder: ArgEncoder[__TypeKind]    = new ArgEncoder[__TypeKind] {
      override def encode(value: __TypeKind): __Value = value match {
        case __TypeKind.SCALAR       => EnumValue("SCALAR")
        case __TypeKind.OBJECT       => EnumValue("OBJECT")
        case __TypeKind.INTERFACE    => EnumValue("INTERFACE")
        case __TypeKind.UNION        => EnumValue("UNION")
        case __TypeKind.ENUM         => EnumValue("ENUM")
        case __TypeKind.INPUT_OBJECT => EnumValue("INPUT_OBJECT")
        case __TypeKind.LIST         => EnumValue("LIST")
        case __TypeKind.NON_NULL     => EnumValue("NON_NULL")
      }
      override def typeName: String                   = "__TypeKind"
    }
  }

  sealed trait __DirectiveLocation extends scala.Product with scala.Serializable
  object __DirectiveLocation {
    case object QUERY                  extends __DirectiveLocation
    case object MUTATION               extends __DirectiveLocation
    case object SUBSCRIPTION           extends __DirectiveLocation
    case object FIELD                  extends __DirectiveLocation
    case object FRAGMENT_DEFINITION    extends __DirectiveLocation
    case object FRAGMENT_SPREAD        extends __DirectiveLocation
    case object INLINE_FRAGMENT        extends __DirectiveLocation
    case object SCHEMA                 extends __DirectiveLocation
    case object SCALAR                 extends __DirectiveLocation
    case object OBJECT                 extends __DirectiveLocation
    case object FIELD_DEFINITION       extends __DirectiveLocation
    case object ARGUMENT_DEFINITION    extends __DirectiveLocation
    case object INTERFACE              extends __DirectiveLocation
    case object UNION                  extends __DirectiveLocation
    case object ENUM                   extends __DirectiveLocation
    case object ENUM_VALUE             extends __DirectiveLocation
    case object INPUT_OBJECT           extends __DirectiveLocation
    case object INPUT_FIELD_DEFINITION extends __DirectiveLocation

    implicit val decoder: ScalarDecoder[__DirectiveLocation] = {
      case __StringValue("QUERY")                  => Right(__DirectiveLocation.QUERY)
      case __StringValue("MUTATION")               => Right(__DirectiveLocation.MUTATION)
      case __StringValue("SUBSCRIPTION")           => Right(__DirectiveLocation.SUBSCRIPTION)
      case __StringValue("FIELD")                  => Right(__DirectiveLocation.FIELD)
      case __StringValue("FRAGMENT_DEFINITION")    => Right(__DirectiveLocation.FRAGMENT_DEFINITION)
      case __StringValue("FRAGMENT_SPREAD")        => Right(__DirectiveLocation.FRAGMENT_SPREAD)
      case __StringValue("INLINE_FRAGMENT")        => Right(__DirectiveLocation.INLINE_FRAGMENT)
      case __StringValue("SCHEMA")                 => Right(__DirectiveLocation.SCHEMA)
      case __StringValue("SCALAR")                 => Right(__DirectiveLocation.SCALAR)
      case __StringValue("OBJECT")                 => Right(__DirectiveLocation.OBJECT)
      case __StringValue("FIELD_DEFINITION")       => Right(__DirectiveLocation.FIELD_DEFINITION)
      case __StringValue("ARGUMENT_DEFINITION")    => Right(__DirectiveLocation.ARGUMENT_DEFINITION)
      case __StringValue("INTERFACE")              => Right(__DirectiveLocation.INTERFACE)
      case __StringValue("UNION")                  => Right(__DirectiveLocation.UNION)
      case __StringValue("ENUM")                   => Right(__DirectiveLocation.ENUM)
      case __StringValue("ENUM_VALUE")             => Right(__DirectiveLocation.ENUM_VALUE)
      case __StringValue("INPUT_OBJECT")           => Right(__DirectiveLocation.INPUT_OBJECT)
      case __StringValue("INPUT_FIELD_DEFINITION") => Right(__DirectiveLocation.INPUT_FIELD_DEFINITION)
      case other                                   => Left(DecodingError(s"Can't build __DirectiveLocation from input $other"))
    }
    implicit val encoder: ArgEncoder[__DirectiveLocation]    = new ArgEncoder[__DirectiveLocation] {
      override def encode(value: __DirectiveLocation): __Value = value match {
        case __DirectiveLocation.QUERY                  => EnumValue("QUERY")
        case __DirectiveLocation.MUTATION               => EnumValue("MUTATION")
        case __DirectiveLocation.SUBSCRIPTION           => EnumValue("SUBSCRIPTION")
        case __DirectiveLocation.FIELD                  => EnumValue("FIELD")
        case __DirectiveLocation.FRAGMENT_DEFINITION    => EnumValue("FRAGMENT_DEFINITION")
        case __DirectiveLocation.FRAGMENT_SPREAD        => EnumValue("FRAGMENT_SPREAD")
        case __DirectiveLocation.INLINE_FRAGMENT        => EnumValue("INLINE_FRAGMENT")
        case __DirectiveLocation.SCHEMA                 => EnumValue("SCHEMA")
        case __DirectiveLocation.SCALAR                 => EnumValue("SCALAR")
        case __DirectiveLocation.OBJECT                 => EnumValue("OBJECT")
        case __DirectiveLocation.FIELD_DEFINITION       => EnumValue("FIELD_DEFINITION")
        case __DirectiveLocation.ARGUMENT_DEFINITION    => EnumValue("ARGUMENT_DEFINITION")
        case __DirectiveLocation.INTERFACE              => EnumValue("INTERFACE")
        case __DirectiveLocation.UNION                  => EnumValue("UNION")
        case __DirectiveLocation.ENUM                   => EnumValue("ENUM")
        case __DirectiveLocation.ENUM_VALUE             => EnumValue("ENUM_VALUE")
        case __DirectiveLocation.INPUT_OBJECT           => EnumValue("INPUT_OBJECT")
        case __DirectiveLocation.INPUT_FIELD_DEFINITION => EnumValue("INPUT_FIELD_DEFINITION")
      }
      override def typeName: String                            = "__DirectiveLocation"
    }
  }

  type __Schema
  object __Schema {
    def description: SelectionBuilder[__Schema, Option[String]]                                                 = Field("description", OptionOf(Scalar()))
    def types[A](innerSelection: SelectionBuilder[__Type, A]): SelectionBuilder[__Schema, List[A]]              =
      Field("types", ListOf(Obj(innerSelection)))
    def queryType[A](innerSelection: SelectionBuilder[__Type, A]): SelectionBuilder[__Schema, A]                =
      Field("queryType", Obj(innerSelection))
    def mutationType[A](innerSelection: SelectionBuilder[__Type, A]): SelectionBuilder[__Schema, Option[A]]     =
      Field("mutationType", OptionOf(Obj(innerSelection)))
    def subscriptionType[A](innerSelection: SelectionBuilder[__Type, A]): SelectionBuilder[__Schema, Option[A]] =
      Field("subscriptionType", OptionOf(Obj(innerSelection)))
    def directives[A](innerSelection: SelectionBuilder[__Directive, A]): SelectionBuilder[__Schema, List[A]]    =
      Field("directives", ListOf(Obj(innerSelection)))
  }

  type __Type
  object __Type {
    def kind: SelectionBuilder[__Type, __TypeKind]                                                                   = Field("kind", Scalar())
    def name: SelectionBuilder[__Type, Option[String]]                                                               = Field("name", OptionOf(Scalar()))
    def description: SelectionBuilder[__Type, Option[String]]                                                        = Field("description", OptionOf(Scalar()))
    def fields[A](
      includeDeprecated: Option[Boolean] = None
    )(innerSelection: SelectionBuilder[__Field, A]): SelectionBuilder[__Type, Option[List[A]]]                       =
      Field(
        "fields",
        OptionOf(ListOf(Obj(innerSelection))),
        arguments = List(Argument("includeDeprecated", includeDeprecated))
      )
    def interfaces[A](innerSelection: SelectionBuilder[__Type, A]): SelectionBuilder[__Type, Option[List[A]]]        =
      Field("interfaces", OptionOf(ListOf(Obj(innerSelection))))
    def possibleTypes[A](innerSelection: SelectionBuilder[__Type, A]): SelectionBuilder[__Type, Option[List[A]]]     =
      Field("possibleTypes", OptionOf(ListOf(Obj(innerSelection))))
    def enumValues[A](
      includeDeprecated: Option[Boolean] = None
    )(innerSelection: SelectionBuilder[__EnumValue, A]): SelectionBuilder[__Type, Option[List[A]]]                   =
      Field(
        "enumValues",
        OptionOf(ListOf(Obj(innerSelection))),
        arguments = List(Argument("includeDeprecated", includeDeprecated))
      )
    def inputFields[A](innerSelection: SelectionBuilder[__InputValue, A]): SelectionBuilder[__Type, Option[List[A]]] =
      Field("inputFields", OptionOf(ListOf(Obj(innerSelection))))
    def ofType[A](innerSelection: SelectionBuilder[__Type, A]): SelectionBuilder[__Type, Option[A]]                  =
      Field("ofType", OptionOf(Obj(innerSelection)))
  }

  type __Field
  object __Field {
    def name: SelectionBuilder[__Field, String]                                                        = Field("name", Scalar())
    def description: SelectionBuilder[__Field, Option[String]]                                         = Field("description", OptionOf(Scalar()))
    def args[A](innerSelection: SelectionBuilder[__InputValue, A]): SelectionBuilder[__Field, List[A]] =
      Field("args", ListOf(Obj(innerSelection)))
    def `type`[A](innerSelection: SelectionBuilder[__Type, A]): SelectionBuilder[__Field, A]           =
      Field("type", Obj(innerSelection))
    def isDeprecated: SelectionBuilder[__Field, Boolean]                                               = Field("isDeprecated", Scalar())
    def deprecationReason: SelectionBuilder[__Field, Option[String]]                                   = Field("deprecationReason", OptionOf(Scalar()))
  }

  type __InputValue
  object __InputValue {
    def name: SelectionBuilder[__InputValue, String]                                              = Field("name", Scalar())
    def description: SelectionBuilder[__InputValue, Option[String]]                               = Field("description", OptionOf(Scalar()))
    def `type`[A](innerSelection: SelectionBuilder[__Type, A]): SelectionBuilder[__InputValue, A] =
      Field("type", Obj(innerSelection))
    def defaultValue: SelectionBuilder[__InputValue, Option[String]]                              = Field("defaultValue", OptionOf(Scalar()))
  }

  type __EnumValue
  object __EnumValue {
    def name: SelectionBuilder[__EnumValue, String]                      = Field("name", Scalar())
    def description: SelectionBuilder[__EnumValue, Option[String]]       = Field("description", OptionOf(Scalar()))
    def isDeprecated: SelectionBuilder[__EnumValue, Boolean]             = Field("isDeprecated", Scalar())
    def deprecationReason: SelectionBuilder[__EnumValue, Option[String]] =
      Field("deprecationReason", OptionOf(Scalar()))
  }

  type __Directive
  object __Directive {
    def name: SelectionBuilder[__Directive, String]                                                        = Field("name", Scalar())
    def description: SelectionBuilder[__Directive, Option[String]]                                         = Field("description", OptionOf(Scalar()))
    def locations: SelectionBuilder[__Directive, List[__DirectiveLocation]]                                = Field("locations", ListOf(Scalar()))
    def args[A](innerSelection: SelectionBuilder[__InputValue, A]): SelectionBuilder[__Directive, List[A]] =
      Field("args", ListOf(Obj(innerSelection)))
    def isRepeatable: SelectionBuilder[__Directive, Boolean]                                               = Field("isRepeatable", Scalar())
  }

  type Query = RootQuery
  object Query {
    def __schema[A](innerSelection: SelectionBuilder[__Schema, A]): SelectionBuilder[RootQuery, A]                   =
      Field("__schema", Obj(innerSelection))
    def __type[A](name: String)(innerSelection: SelectionBuilder[__Type, A]): SelectionBuilder[RootQuery, Option[A]] =
      Field("__type", OptionOf(Obj(innerSelection)), arguments = List(Argument("name", name)))
  }

}
