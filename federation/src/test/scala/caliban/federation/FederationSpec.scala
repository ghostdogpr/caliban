package caliban.federation

import caliban.CalibanError
import caliban.CalibanError.ValidationError
import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.TestUtils._
import caliban.Value.{ BooleanValue, StringValue }
import caliban.schema.Annotations.GQLDirective
import caliban.schema.Schema
import zio.test.Assertion._
import zio.test._
import zio.query.ZQuery

object FederationSpec extends DefaultRunnableSpec {
  case class OrphanChild(id: String)

  object OrphanChild {
    implicit val schema: Schema[Any, Orphan] = Schema.gen
  }

  @GQLDirective(Key("name"))
  @GQLDirective(Extend)
  case class Orphan(@GQLDirective(External) name: String, nicknames: List[String], child: OrphanChild)

  object Orphan {
    implicit val schema: Schema[Any, Orphan] = Schema.gen
  }

  case class OrphanArgs(name: String)

  val entityResolver =
    EntityResolver[Any, CharacterArgs, Character](args => ZQuery.succeed(characters.find(_.name == args.name)))

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
    testM("should resolve federated types") {
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

      assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
        equalTo("""{"_entities":[{"__typename":"Character","name":"Amos Burton"}]}""")
      )
    },
    testM("should not include _entities if not resolvers provided") {
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

      assertM(interpreter.flatMap(_.execute(query)).map(_.errors))(
        equalTo(
          List(
            ValidationError(
              "Field '_entities' does not exist on type 'Query'.",
              "The target field of a field selection must be defined on the scoped type of the selection set. There are no limitations on alias names."
            )
          )
        )
      )
    },
    testM("should include orphan entities in sdl") {
      val interpreter = (graphQL(resolver) @@ federated(orphanResolver)).interpreter

      val query = gqldoc("""{ _service { sdl } }""")
      assertM(interpreter.flatMap(_.execute(query)).map(d => d.data.toString))(
        containsString(
          """type Orphan @extends @key(fields: \"name\") {\n  name: String! @external\n  nicknames: [String!]!\n  child: OrphanChild!\n}"""
        ) &&
          containsString(
            """type OrphanChild {\n  id: String!\n}"""
          )
      )
    },
    testM("should include field metadata") {
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
