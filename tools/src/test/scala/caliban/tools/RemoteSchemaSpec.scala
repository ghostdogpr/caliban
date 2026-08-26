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

import scala.util.Try

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

  def spec = suite("RemoteSchemaSpec")(
    test("reports a built-in scalar used as a root type without throwing") {
      val result = Try {
        Parser
          .parseQuery("schema { query: String } type Foo { id: ID }")
          .flatMap(RemoteSchema.toRootType(_))
      }

      assertTrue(result.isSuccess, result.toOption.exists(_.isLeft))
    },
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
          |type Query { node: Node }
          |interface Node { id: ID! }
          |type Product implements Node {
          |  id: ID!
          |  legacy: String @deprecated
          |  url: URL
          |}
          |scalar URL @specifiedBy(url: "https://example.com/url")
          |""".stripMargin

      for {
        document <- ZIO.fromEither(Parser.parseQuery(schema))
        rootType <- ZIO.fromEither(RemoteSchema.toRootType(document))
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
        url.flatMap(_.specifiedByURL).contains("https://example.com/url")
      )
    },
    test("builds a validated RootType from conventional roots and extensions") {
      val schema =
        """
          |type Query {
          |  value: String
          |}
          |
          |extend type Query {
          |  version: String
          |}
          |
          |type Mutation {
          |  update: Boolean
          |}
          |
          |type Subscription {
          |  events: String
          |}
          |""".stripMargin

      for {
        document <- ZIO.fromEither(Parser.parseQuery(schema))
        rootType <- ZIO.fromEither(RemoteSchema.toRootType(document))
        fields    = rootType.queryType.fields(__DeprecatedArgs()).toList.flatten.map(_.name)
      } yield assertTrue(
        rootType.queryType.name.contains("Query"),
        rootType.mutationType.flatMap(_.name).contains("Mutation"),
        rootType.subscriptionType.flatMap(_.name).contains("Subscription"),
        fields == List("value", "version")
      )
    },
    test("retains conventional roots when schema metadata is supplied by an extension") {
      val schema =
        """
          |extend schema @link(url: "https://specs.apollo.dev/federation/v2.3")
          |type Query { value: String }
          |type Mutation { update: Boolean }
          |type Subscription { events: String }
          |""".stripMargin

      for {
        document <- ZIO.fromEither(Parser.parseQuery(schema))
        rootType <- ZIO.fromEither(RemoteSchema.toRootType(document))
      } yield assertTrue(
        rootType.queryType.name.contains("Query"),
        rootType.mutationType.flatMap(_.name).contains("Mutation"),
        rootType.subscriptionType.flatMap(_.name).contains("Subscription")
      )
    },
    test("rejects conflicting operation roots across schema declarations") {
      val schema =
        """
          |schema { query: Query }
          |extend schema { query: RootQuery }
          |type Query { value: String }
          |type RootQuery { value: String }
          |""".stripMargin

      for {
        document <- ZIO.fromEither(Parser.parseQuery(schema))
        result    = RemoteSchema.toRootType(document)
      } yield assertTrue(
        result.left.exists(_.msg == "Conflicting query root types are declared: 'Query', 'RootQuery'.")
      )
    },
    test("does not infer Query when a schema definition omits the query root") {
      val schema =
        """
          |schema { mutation: Mutation }
          |type Query { value: String }
          |type Mutation { update: Boolean }
          |""".stripMargin

      for {
        document <- ZIO.fromEither(Parser.parseQuery(schema))
        result    = RemoteSchema.toRootType(document)
      } yield assertTrue(result.left.exists(_.msg == "The query root operation is missing."))
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
        validDocument   <- ZIO.fromEither(Parser.parseQuery(validSchema))
        invalidDocument <- ZIO.fromEither(Parser.parseQuery(invalidSchema))
        rootType        <- ZIO.fromEither(RemoteSchema.toRootType(validDocument))
        oneOf            = rootType.additionalTypes.find(_.name.contains("Choice")).flatMap(_.isOneOf)
        invalid          = RemoteSchema.toRootType(invalidDocument)
      } yield assertTrue(oneOf.contains(true), invalid.isLeft)
    },
    test("rejects multiple schema definitions") {
      val schema =
        """
          |schema { query: Query }
          |schema { query: Query }
          |type Query { value: String }
          |""".stripMargin

      for {
        document <- ZIO.fromEither(Parser.parseQuery(schema))
        result    = RemoteSchema.toRootType(document)
      } yield assertTrue(result.left.exists(_.msg == "Schema is defined multiple times."))
    },
    test("rejects a type shared by multiple root operations") {
      val schema =
        """
          |schema { query: Root mutation: Root }
          |type Root { value: String }
          |""".stripMargin

      for {
        document <- ZIO.fromEither(Parser.parseQuery(schema))
        result    = RemoteSchema.toRootType(document)
      } yield assertTrue(result.left.exists(_.msg == "Root operation type 'Root' is used more than once."))
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
