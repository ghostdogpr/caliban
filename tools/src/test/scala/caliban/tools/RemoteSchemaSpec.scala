package caliban.tools

import caliban._
import caliban.introspection.adt._
import caliban.parsing.Parser
import caliban.schema._
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._
import zio._
import zio.test.Assertion._
import zio.test._
import caliban.schema.Annotations._
import caliban.Macros.gqldoc
import caliban.execution.Feature
import caliban.transformers.Transformer

object RemoteSchemaSpec extends ZIOSpecDefault {
  sealed trait EnumType  extends Product with Serializable
  case object EnumValue1 extends EnumType
  case object EnumValue2 extends EnumType

  sealed trait UnionType                extends Product with Serializable
  case class UnionValue1(field: String) extends UnionType

  case class Args(@GQLDeprecated("Use nameV2") name: String = "defaultValue", nameV2: String)

  case class Object(
    field: Int,
    optionalField: Option[Float],
    withDefault: Option[String] = Some("defaultValue"),
    enumField: EnumType,
    unionField: UnionType
  )

  object Resolvers {
    def getObject(args: Args): Object =
      Object(
        field = 1,
        optionalField = None,
        enumField = EnumValue1,
        unionField = UnionValue1("value")
      )
  }

  case class Queries(
    getObject: Args => Object
  )

  val queries = Queries(
    getObject = Resolvers.getObject
  )

  val api = graphQL(
    RootResolver(queries)
  )

  private def normalize(schema: String): Either[CalibanError, RootType] =
    Parser.parseQuery(schema).flatMap(RemoteSchema.normalize(_).map(_.rootType))

  def spec = suite("RemoteSchemaSpec")(
    test("is isomorphic") {
      for {
        introspected <- SchemaLoader.fromCaliban(api).load
        remoteSchema <- ZIO.fromOption(RemoteSchema.parseRemoteSchema(introspected))
        remoteAPI    <- ZIO.succeed(fromRemoteSchema(remoteSchema))
        sdl           = api.render
        remoteSDL     = remoteAPI.render
        res          <- SchemaComparison.compare(
                          SchemaLoader.fromCaliban(api),
                          SchemaLoader.fromCaliban(remoteAPI)
                        )
      } yield assertTrue(res.isEmpty, sdl == remoteSDL)
    },
    test("properly resolves interface types") {
      @GQLInterface
      sealed trait Node

      sealed trait Viewer
      case class User(id: String, email: String) extends Node with Viewer
      case class Superuser(id: String)           extends Node with Viewer

      case class Queries(
        whoAmI: Node = User("1", "foo@bar.com")
      )

      val api   = graphQL(RootResolver(Queries()))
      val query = gqldoc("""
             query {
               whoAmI {
                 ...on User {
                   email
                 }
                 ...on Node {
                   id
                 }
               }
              }""")

      for {
        introspected <- SchemaLoader.fromCaliban(api).load
        remoteSchema <- ZIO.fromOption(RemoteSchema.parseRemoteSchema(introspected))
        remoteAPI    <- ZIO.succeed(fromRemoteSchema(remoteSchema))
        interpreter  <- remoteAPI.interpreter
        res          <- interpreter.check(query)
      } yield assert(res)(isUnit)
    },
    test("preserves subscription type from schema definition") {
      val schema =
        """
          |schema {
          |  query: Query
          |  subscription: Subscription
          |}
          |
          |type Query {
          |  version: String
          |}
          |
          |type Subscription {
          |  tick: Int
          |}
          |""".stripMargin

      for {
        doc          <- ZIO.fromEither(Parser.parseQuery(schema))
        remoteSchema <- ZIO.fromOption(RemoteSchema.parseRemoteSchema(doc))
      } yield assertTrue(
        remoteSchema.subscriptionType.flatMap(_.name).contains("Subscription"),
        remoteSchema.subscriptionType.flatMap(_.fields(__DeprecatedArgs()).map(_.map(_.name))).contains(List("tick"))
      )
    },
    test("parseRemoteSchema preserves deprecated fields and arguments by default") {
      val schema =
        """
          |schema { query: Query }
          |type Query {
          |  legacy(old: String @deprecated): String @deprecated
          |}
          |""".stripMargin

      for {
        document     <- ZIO.fromEither(Parser.parseQuery(schema))
        remoteSchema <- ZIO.fromOption(RemoteSchema.parseRemoteSchema(document))
        defaultFields = remoteSchema.queryType.fields(__DeprecatedArgs()).getOrElse(Nil)
        allFields     = remoteSchema.queryType.fields(__DeprecatedArgs(Some(true))).getOrElse(Nil)
        hiddenFields  = remoteSchema.queryType.fields(__DeprecatedArgs(Some(false))).getOrElse(Nil)
        legacy        = allFields.find(_.name == "legacy")
        defaultArgs   = legacy.toList.flatMap(_.args(__DeprecatedArgs()))
        hiddenArgs    = legacy.toList.flatMap(_.args(__DeprecatedArgs(Some(false))))
      } yield assertTrue(
        defaultFields.exists(_.name == "legacy"),
        defaultArgs.exists(_.name == "old"),
        !hiddenFields.exists(_.name == "legacy"),
        !hiddenArgs.exists(_.name == "old")
      )
    },
    test("preserves interface-implements-interface relationships") {
      val schema =
        """
          |schema { query: Query }
          |type Query { node: Node }
          |interface Node { id: ID! }
          |interface Resource implements Node { id: ID! name: String! }
          |type File implements Resource & Node { id: ID! name: String! }
          |""".stripMargin

      for {
        doc          <- ZIO.fromEither(Parser.parseQuery(schema))
        remoteSchema <- ZIO.fromOption(RemoteSchema.parseRemoteSchema(doc))
        resource      = remoteSchema.types.find(_.name.contains("Resource"))
        implemented   = resource.flatMap(_.interfaces()).getOrElse(Nil).flatMap(_.name)
      } yield assertTrue(implemented.contains("Node"))
    },
    test("preserves metadata on object types reached through an interface") {
      val schema =
        """
          |type Query { node: Node status: Status }
          |"Product status" enum Status { ACTIVE }
          |interface Node { id: ID! }
          |type Product implements Node {
          |  id: ID!
          |  legacy: String @deprecated
          |  url: URL
          |}
          |scalar URL @specifiedBy(url: "https://example.com/url")
          |""".stripMargin

      for {
        rootType <- ZIO.fromEither(normalize(schema))
        product   = rootType.types
                      .get("Node")
                      .flatMap(_.possibleTypes)
                      .flatMap(_.find(_.name.contains("Product")))
        visible   = product.flatMap(_.fields(__DeprecatedArgs())).getOrElse(Nil)
        all       = product.flatMap(_.fields(__DeprecatedArgs(Some(true)))).getOrElse(Nil)
        legacy    = all.find(_.name == "legacy")
        url       = all.find(_.name == "url").map(_._type.innerType)
      } yield assertTrue(
        !visible.exists(_.name == "legacy"),
        legacy.flatMap(_.deprecationReason).contains("No longer supported"),
        url.flatMap(_.specifiedByURL).contains("https://example.com/url"),
        rootType.types.get("Status").flatMap(_.description).contains("Product status")
      )
    },
    test("builds a validated RootType from conventional roots and extensions, with or without schema metadata") {
      val types    =
        """
          |type Query { value: String }
          |extend type Query { version: String }
          |type Mutation { update: Boolean }
          |type Subscription { events: String }
          |""".stripMargin
      val withLink = "extend schema @link(url: \"https://specs.apollo.dev/federation/v2.3\")\n" + types

      ZIO.foreach(List(types, withLink))(schema => ZIO.fromEither(normalize(schema))).map { rootTypes =>
        assertTrue(rootTypes.forall { rootType =>
          rootType.queryType.name.contains("Query") &&
          rootType.mutationType.flatMap(_.name).contains("Mutation") &&
          rootType.subscriptionType.flatMap(_.name).contains("Subscription") &&
          rootType.queryType.fields(__DeprecatedArgs()).toList.flatten.map(_.name) == List("value", "version")
        })
      }
    },
    test("preserves and validates OneOf input objects") {
      val validSchema   =
        """
          |type Query { find(by: Choice!): String }
          |input Choice @oneOf { id: ID name: String }
          |""".stripMargin
      val invalidSchema =
        """
          |type Query { find(by: Choice!): String }
          |input Choice @oneOf { id: ID! }
          |""".stripMargin

      for {
        rootType <- ZIO.fromEither(normalize(validSchema))
        oneOf     = rootType.additionalTypes.find(_.name.contains("Choice")).flatMap(_.isOneOf)
      } yield assertTrue(oneOf.contains(true), normalize(invalidSchema).isLeft)
    },
    test("rejects invalid schema documents with a precise error") {
      val cases = List(
        "schema { query: String } type Foo { id: ID }"                                                                                      -> "The query root type 'String' must be an object type.",
        "schema { query: Query } extend schema { query: RootQuery } type Query { value: String } type RootQuery { value: String }"          ->
          "Conflicting query root types are declared: 'Query', 'RootQuery'.",
        "schema { mutation: Mutation } type Query { value: String } type Mutation { update: Boolean }"                                      ->
          "The query root operation is missing.",
        "schema { mutation: Mutation } type Mutation { update: Missing } type Duplicate { value: String } type Duplicate { value: String }" ->
          "The query root operation is missing.",
        "type Mutation { update: Boolean }"                                                                                                 -> "The query root operation is missing.",
        "schema { query: Query } schema { query: Query } type Query { value: String }"                                                      -> "Schema is defined multiple times.",
        "schema { query: Root mutation: Root } type Root { value: String }"                                                                 -> "Root operation type 'Root' is used more than once."
      )

      assertTrue(
        cases.map { case (schema, _) => normalize(schema).left.toOption.map(_.msg) } ==
          cases.map { case (_, message) => Some(message) }
      )
    }
  )

  def fromRemoteSchema(s: __Schema): GraphQL[Any] =
    new GraphQL[Any] {
      override protected val schemaBuilder                                 =
        RootSchemaBuilder(
          query = Some(
            Operation[Any](
              s.queryType,
              Step.NullStep
            )
          ),
          mutation = None,
          subscription = None
        )
      override protected val additionalDirectives: List[__Directive]       = List()
      override protected val wrappers: List[caliban.wrappers.Wrapper[Any]] = List()
      override protected val features: Set[Feature]                        = Set.empty
      override protected val transformer: Transformer[Any]                 = Transformer.empty
    }

}
