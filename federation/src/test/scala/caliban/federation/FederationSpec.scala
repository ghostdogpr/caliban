package caliban.federation

import caliban.CalibanError.ValidationError
import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.TestUtils._
import caliban.schema.Annotations.GQLDirective
import caliban.schema.Schema
import zio.test.Assertion._
import zio.test._
import zio.query.ZQuery

object FederationSpec extends DefaultRunnableSpec {
  case class OrphanChild(id: String)

  object OrphanChild {
    implicit val schema: Schema[Any, Orphan] = Schema.gen[Orphan]
  }

  @GQLDirective(Key("name"))
  @GQLDirective(Extend)
  case class Orphan(@GQLDirective(External) name: String, nicknames: List[String], child: OrphanChild)

  object Orphan {
    implicit val schema: Schema[Any, Orphan] = Schema.gen[Orphan]
  }

  case class OrphanArgs(name: String)

  val entityResolver =
    EntityResolver[Any, CharacterArgs, Character](args => ZQuery.succeed(characters.find(_.name == args.name)))

  val orphanResolver =
    EntityResolver[Any, OrphanArgs, Orphan](args =>
      ZQuery.succeed(characters.find(_.name == args.name).map(c => Orphan(c.name, c.nicknames, OrphanChild("abc"))))
    )

  override def spec = suite("FederationSpec")(
    testM("should resolve federated types") {
      val interpreter = federate(graphQL(resolver), entityResolver).interpreter

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
      val interpreter = federate(graphQL(resolver)).interpreter

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
      val interpreter = federate(graphQL(resolver), orphanResolver).interpreter

      val query = gqldoc("""{ _service { sdl } }""")
      assertM(interpreter.flatMap(_.execute(query)).map(d => d.data.toString))(
        containsString(
          """type Orphan @key(fields: \"name\") @extends {\n  name: String! @external\n  nicknames: [String!]!\n  child: OrphanChild!\n}"""
        ) &&
          containsString(
            """type OrphanChild {\n  id: String!\n}"""
          )
      )
    }
  )
}
