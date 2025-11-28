package caliban.relay

import caliban.CalibanError.ExecutionError
import caliban.Value.StringValue
import caliban.rendering.DocumentRenderer
import caliban.{ graphQL, CalibanError, GraphQLResponse, InputValue, ResponseValue, RootResolver, Value }
import zio.test.{ assertTrue, assertZIO, ZIOSpecDefault }
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._
import caliban.schema.{ ArgBuilder, Schema }
import zio.query.ZQuery
import zio.test.Assertion.equalTo

object GlobalIdentifierSpec extends ZIOSpecDefault {
  case class ID(value: String)

  implicit val schema: caliban.schema.Schema[Any, ID] = Schema.scalarSchema(
    "ID",
    None,
    None,
    None,
    id => StringValue(id.value)
  )

  implicit val argBuilder: caliban.schema.ArgBuilder[ID] =
    new ArgBuilder[ID] {
      override def build(input: InputValue): Either[ExecutionError, ID] =
        input match {
          case StringValue(value) => Right(ID(value))
          case _                  => Left(ExecutionError("Expected a string"))
        }
    }

  implicit val typeResolver: TypeResolver[ID] = new TypeResolver[ID] {
    override def resolve(a: ID): Either[ExecutionError, String] =
      a.value.split(":") match {
        case Array(typename, _) => Right(typename)
        case _                  => Left(ExecutionError("Invalid id"))
      }
  }

  case class Ship(id: ID, name: String, purpose: String)
  case class Character(id: ID, name: String)
  case class Query(
    characters: List[Character],
    ships: List[Ship]
  )

  val characters = List(
    Character(ID("1"), "James Holden"),
    Character(ID("2"), "Naomi Nagata"),
    Character(ID("3"), "Amos Burton"),
    Character(ID("4"), "Alex Kamal")
  )

  val ships = List(
    Ship(ID("1"), "Rocinante", "Stealth Frigate"),
    Ship(ID("2"), "Canterbury", "Destroyer"),
    Ship(ID("3"), "Nauvoo", "Generation Ship"),
    Ship(ID("4"), "Behemoth", "Belter Ship")
  )

  val shipResolver: NodeResolver[Any, ID] = NodeResolver.from[ID] { id =>
    ZQuery.succeed(ships.find(s => id.value.endsWith(s.id.value)).map(_.copy(id = id)))
  }

  val characterResolver: NodeResolver[Any, ID] = NodeResolver.from[ID] { id =>
    ZQuery.succeed(characters.find(c => id.value.endsWith(c.id.value)).map(_.copy(id = id)))
  }

  val api = graphQL(
    RootResolver(
      Query(
        characters = characters,
        ships = ships
      )
    )
  )

  val spec = suite("GlobalIdentifierSpec")(
    test("augment schema with node field") {
      val augmented = api @@
        RelaySupport.withGlobalIdentifiers(shipResolver, characterResolver)

      assertTrue(
        DocumentRenderer.renderCompact(
          augmented.toDocument
        ) == "schema{query:Query}interface Node{id:ID!} type Character implements Node{id:ID! name:String!} type Query{characters:[Character!]! ships:[Ship!]! node(id:ID!):Node} type Ship implements Node{id:ID! name:String! purpose:String!} "
      )
    },
    test("resolve node field") {
      val augmented = api @@
        RelaySupport.withGlobalIdentifiers[ID](shipResolver, characterResolver)

      val query =
        """{
          | character: node(id:"Character:2"){
          |   id,...on Character{name}
          | }
          | ship: node(id:"Ship:3"){
          |   id,...on Ship{purpose}
          | }
          | missing: node(id:"Ship:5"){
          |  id,...on Ship{purpose}
          | }
          |}""".stripMargin

      val result = augmented.interpreterUnsafe.execute(query)

      assertZIO(result)(
        equalTo(
          GraphQLResponse[CalibanError](
            data = ResponseValue.ObjectValue(
              List(
                "character" -> ResponseValue.ObjectValue(
                  List("id" -> Value.StringValue("Character:2"), "name" -> Value.StringValue("Naomi Nagata"))
                ),
                "ship"      -> ResponseValue.ObjectValue(
                  List("id" -> Value.StringValue("Ship:3"), "purpose" -> Value.StringValue("Generation Ship"))
                ),
                "missing"   -> Value.NullValue
              )
            ),
            errors = Nil
          )
        )
      )
    }
  )

}
