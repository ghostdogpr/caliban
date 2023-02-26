package caliban.federation

import caliban.CalibanError
import caliban.CalibanError.ValidationError
import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.TestUtils._
import caliban.Value.{ BooleanValue, StringValue }
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._
import caliban.federation.v1._
import zio.test.Assertion._
import zio.test._
import zio.query.ZQuery

object FederationV1Spec extends ZIOSpecDefault {
  case class OrphanChild(id: String)

  @GQLKey("name")
  @GQLExtend
  case class Orphan(@GQLExternal name: String, nicknames: List[String], child: OrphanChild)

  case class OrphanArgs(name: String)

  val entityResolver =
    EntityResolver[Any, CharacterArgs, Character] { args =>
      val res = characters.find(_.name == args.name)
      if (res.isDefined) ZQuery.succeed(res) else ZQuery.fail(CalibanError.ExecutionError("not found"))
    }

  val orphanResolver =
    EntityResolver[Any, OrphanArgs, Orphan](args =>
      ZQuery.succeed(characters.find(_.name == args.name).map(c => Orphan(c.name, c.nicknames, OrphanChild("abc"))))
    )

  val functionEntityResolver =
    EntityResolver.fromMetadata[CharacterArgs](field =>
      args =>
        ZQuery.fromEither(
          Either.cond(
            !field.fields.exists(_.name == "nicknames"),
            characters.find(_.name == args.name),
            CalibanError.ExecutionError("AAAAAAHHHHH")
          )
        )
    )

  override def spec = suite("FederationSpec")(
    test("should resolve federated types") {
      val interpreter = (graphQL(resolver) @@ federated(entityResolver)).interpreter

      val query = gqldoc("""
            query test {
              _entities(representations: [{__typename: "Character", name: "Amos Burton"}]) {
                  __typename
                  ... on Character {
                    name
                  }
              }
            }""")

      interpreter.flatMap(_.execute(query)).map { response =>
        assertTrue(response.data.toString == """{"_entities":[{"__typename":"Character","name":"Amos Burton"}]}""")
      }
    },
    test("should resolve federated types and return partial responses") {
      val interpreter = (graphQL(resolver) @@ federated(entityResolver)).interpreter

      val query = gqldoc("""
            query test {
              _entities(representations: [{__typename: "Character", name: "Amos Burton"},{__typename: "Character", name: "Nothing to see here"}]) {
                  __typename
                  ... on Character {
                    name
                  }
              }
            }""")

      interpreter.flatMap(_.execute(query)).map { response =>
        assertTrue(
          response.data.toString == """{"_entities":[{"__typename":"Character","name":"Amos Burton"},null]}"""
        ) &&
        assertTrue(
          response.errors.toString == """List(Execution Error: not found )"""
        )
      }
    },
    test("should not include _entities if not resolvers provided") {
      val interpreter = (graphQL(resolver) @@ federated).interpreter

      val query = gqldoc("""
            query test {
              _entities(representations: [{__typename: "Character", name: "Amos Burton"}]) {
                  __typename
                  ... on Character {
                    name
                  }
              }
            }""")

      interpreter.flatMap(_.execute(query)).map { response =>
        assertTrue(
          response.errors == List(
            ValidationError(
              "Field '_entities' does not exist on type 'Query'.",
              "The target field of a field selection must be defined on the scoped type of the selection set. There are no limitations on alias names."
            )
          )
        )
      }
    },
    test("should include orphan entities in sdl") {
      val interpreter = (graphQL(resolver) @@ federated(orphanResolver)).interpreter

      val query = gqldoc("""{ _service { sdl } }""")
      interpreter
        .flatMap(_.execute(query))
        .map(d => d.data.toString)
        .map(
          assert(_)(
            containsString(
              """type Orphan @extends @key(fields: \"name\") {\n  name: String! @external\n  nicknames: [String!]!\n  child: OrphanChild!\n}"""
            ) &&
              containsString(
                """type OrphanChild {\n  id: String!\n}"""
              )
          )
        )
    },
    test("should include field metadata") {
      val interpreter = (graphQL(resolver) @@ federated(functionEntityResolver)).interpreter
      val query       = gqldoc("""
           query Entities($withNicknames: Boolean = false) {
            _entities(representations: [{__typename: "Character", name: "Amos Burton"}]) {
              ... on Character {
                name
                nicknames @include(if: $withNicknames)
              }
            }
          }
          """)

      def runQuery(withNicknames: Boolean) =
        interpreter.flatMap(_.execute(query, variables = Map("withNicknames" -> BooleanValue(withNicknames))))

      runQuery(true).zipWith(runQuery(false)) { (bad, good) =>
        assertTrue(bad.errors.map(_.msg) == List("AAAAAAHHHHH")) && assertTrue(
          good.data == ObjectValue(
            List("_entities" -> ListValue(List(ObjectValue(List("name" -> StringValue("Amos Burton"))))))
          )
        )
      }
    }
  )
}
