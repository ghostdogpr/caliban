package caliban.execution

import java.util.UUID
import caliban.CalibanError.ExecutionError
import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.{ GraphQL, RootResolver }
import caliban.TestUtils._
import caliban.Value.{ BooleanValue, StringValue }
import caliban.introspection.adt.__Type
import caliban.parsing.adt.LocationInfo
import caliban.schema.Annotations.{ GQLInterface, GQLName }
import caliban.schema.{ Schema, Step, Types }
import zio.{ IO, Task, UIO, ZIO }
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object ExecutionSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("ExecutionSpec")(
      testM("skip directive") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
            query test {
              amos: character(name: "Amos Burton") {
                name
                nicknames @skip(if: true)
              }
            }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"amos":{"name":"Amos Burton"}}""")
        )
      },
      testM("simple query with fields") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
            {
              characters {
                name
              }
            }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo(
            """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
          )
        )
      },
      testM("arguments") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
            {
              characters(origin: MARS) {
                name
                nicknames
              }
            }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo(
            """{"characters":[{"name":"Alex Kamal","nicknames":[]},{"name":"Roberta Draper","nicknames":["Bobbie","Gunny"]}]}"""
          )
        )
      },
      testM("arguments with list coercion") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
            {
              charactersIn(names: "Alex Kamal") {
                name
              }
            }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"charactersIn":[{"name":"Alex Kamal"}]}""")
        )
      },
      testM("aliases") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
             {
               amos: character(name: "Amos Burton") {
                 name
                 nicknames
               },
               naomi: character(name: "Naomi Nagata") {
                 name
                 nicknames
               },
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"amos":{"name":"Amos Burton","nicknames":[]},"naomi":{"name":"Naomi Nagata","nicknames":[]}}""")
        )
      },
      testM("fragment") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
             {
               amos: character(name: "Amos Burton") {
                 ...info
               }
             }

             fragment info on Character {
               name
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"amos":{"name":"Amos Burton"}}""")
        )
      },
      testM("fragment on union") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
             {
               amos: character(name: "Amos Burton") {
                 role {
                   ...roleF
                 }
               }
             }

             fragment roleF on Role {
               ... on Mechanic {
                 shipName
               }
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"amos":{"role":{"shipName":"Rocinante"}}}""")
        )
      },
      testM("inline fragment") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
             {
               amos: character(name: "Amos Burton") {
                 name
                 role {
                   ... on Mechanic {
                     shipName
                   }
                 }
               }
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"amos":{"name":"Amos Burton","role":{"shipName":"Rocinante"}}}""")
        )
      },
      testM("effectful query") {
        val interpreter = graphQL(resolverIO).interpreter
        val query       = gqldoc("""
               {
                 characters {
                   name
                 }
               }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo(
            """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
          )
        )
      },
      testM("mutation") {
        val interpreter = graphQL(resolverWithMutation).interpreter
        val query       = gqldoc("""
               mutation {
                 deleteCharacter(name: "Amos Burton")
               }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(equalTo("""{"deleteCharacter":{}}"""))
      },
      testM("variable") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
             query test($name: String!){
               amos: character(name: $name) {
                 name
               }
             }""")

        assertM(
          interpreter.flatMap(_.execute(query, None, Map("name" -> StringValue("Amos Burton")))).map(_.data.toString)
        )(equalTo("""{"amos":{"name":"Amos Burton"}}"""))
      },
      testM("tolerate missing variables") {
        import io.circe.syntax._

        case class Args(term: String, id: Option[String])
        case class Test(getId: Args => Option[String])
        val api   = graphQL(RootResolver(Test(_.id)))
        val query = """query test($term: String!, $id: String) { getId(term: $term, id: $id) }"""

        assertM(
          api.interpreter.flatMap(_.execute(query, None, Map("term" -> StringValue("search")))).map(_.asJson.noSpaces)
        )(equalTo("""{"data":{"getId":null}}"""))
      },
      testM("field function") {
        import io.circe.syntax._

        case class Character(name: String = "Bob")
        case class Test(character: Field => Character)
        val api   = graphQL(RootResolver(Test(field => Character())))
        val query = """query test { character { name } }"""

        assertM(
          api.interpreter.flatMap(_.execute(query, None, Map())).map(_.asJson.noSpaces)
        )(equalTo("""{"data":{"character":{"name":"Bob"}}}"""))
      },
      testM("field function with input") {
        import io.circe.syntax._

        case class NameInput(name: String)
        case class Character(name: String)
        case class Test(character: Field => NameInput => Character)
        val api   = graphQL(RootResolver(Test(field => input => Character(input.name))))
        val query = """query test { character(name: "Bob") { name }}"""

        assertM(
          api.interpreter.flatMap(_.execute(query, None, Map())).map(_.asJson.noSpaces)
        )(equalTo("""{"data":{"character":{"name":"Bob"}}}"""))
      },
      testM("error on missing required variables") {
        import io.circe.syntax._

        case class Args(term: String, id: String)
        case class Test(getId: Args => String)
        val api   = graphQL(RootResolver(Test(_.id)))
        val query = """query test($term: String!, $id: String!) { getId(term: $term, id: $id) }"""

        assertM(
          api.interpreter.flatMap(_.execute(query, None, Map("term" -> StringValue("search")))).map(_.asJson.noSpaces)
        )(
          equalTo(
            """{"data":null,"errors":[{"message":"Can't build a String from input null","locations":[{"line":1,"column":44}],"path":["getId"]}]}"""
          )
        )
      },
      testM("variable in list") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
             query test($name: String) {
               amos: charactersIn(names: [$name]){
                 name
               }
              }""")

        assertM(
          interpreter.flatMap(_.execute(query, None, Map("name" -> StringValue("Amos Burton")))).map(_.data.toString)
        )(equalTo("""{"amos":[{"name":"Amos Burton"}]}"""))
      },
      testM("variable in object") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
             query test($name: String) {
               exists(character: { name: $name, nicknames: [], origin: EARTH })
              }""")

        assertM(
          interpreter.flatMap(_.execute(query, None, Map("name" -> StringValue("Amos Burton")))).map(_.data.toString)
        )(equalTo("""{"exists":false}"""))
      },
      testM("skip directive") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
             query test{
               amos: character(name: "Amos Burton") {
                 name
                 nicknames @skip(if: true)
               }
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"amos":{"name":"Amos Burton"}}""")
        )
      },
      testM("include directive") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
             query test($included: Boolean!){
               amos: character(name: "Amos Burton") {
                 name
                 nicknames @include(if: $included)
               }
             }""")

        assertM(
          interpreter.flatMap(_.execute(query, None, Map("included" -> BooleanValue(false)))).map(_.data.toString)
        )(equalTo("""{"amos":{"name":"Amos Burton"}}"""))
      },
      testM("test Map") {
        case class Test(map: Map[Int, String])
        val interpreter = graphQL(RootResolver(Test(Map(3 -> "ok")))).interpreter
        val query       = gqldoc("""
             {
               map {
                 key
                 value
               }
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"map":[{"key":3,"value":"ok"}]}""")
        )
      },
      testM("test Either") {
        case class Test(either: Either[Int, String])
        val interpreter = graphQL(RootResolver(Test(Right("ok")))).interpreter
        val query       = gqldoc("""
             {
               either {
                 left
                 right
               }
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"either":{"left":null,"right":"ok"}}""")
        )
      },
      testM("test UUID") {
        case class IdArgs(id: UUID)
        case class Queries(test: IdArgs => UUID)
        val interpreter = graphQL(RootResolver(Queries(_.id))).interpreter
        val query       = gqldoc("""
             {
               test(id: "be722453-d97d-48c2-b535-9badd1b5d4c9")
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"test":"be722453-d97d-48c2-b535-9badd1b5d4c9"}""")
        )
      },
      testM("mapError") {
        import io.circe.syntax._
        case class Test(either: Either[Int, String])
        val api   = graphQL(RootResolver(Test(Right("ok"))))
        val query = """query{}"""

        for {
          interpreter <- api.interpreter
          result      <- interpreter.mapError(_ => "my custom error").execute(query)
        } yield assert(result.errors)(equalTo(List("my custom error"))) &&
          assert(result.asJson.noSpaces)(equalTo("""{"data":null,"errors":[{"message":"my custom error"}]}"""))
      },
      testM("merge 2 APIs") {
        case class Test(name: String)
        case class Test2(id: Int)
        val api1        = graphQL(RootResolver(Test("name")))
        val api2        = graphQL(RootResolver(Test2(2)))
        val interpreter = (api1 |+| api2).interpreter
        val query       =
          """query{
            |  name
            |  id
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(equalTo("""{"name":"name","id":2}"""))
      },
      testM("error path") {
        case class A(b: B)
        case class B(c: IO[Throwable, Int])
        case class Test(a: A)
        val e           = new Exception("boom")
        val interpreter = graphQL(RootResolver(Test(A(B(IO.fail(e)))))).interpreter
        val query       = gqldoc("""
              {
                a {
                  b {
                    c
                  }
                }
              }""")
        assertM(interpreter.flatMap(_.execute(query)).map(_.errors))(
          equalTo(
            List(
              ExecutionError(
                "Effect failure",
                List(Left("a"), Left("b"), Left("c")),
                Some(LocationInfo(21, 5)),
                Some(e)
              )
            )
          )
        )
      },
      testM("ZStream used in a query") {
        case class Queries(test: ZStream[Any, Throwable, Int])
        val interpreter = graphQL(RootResolver(Queries(ZStream(1, 2, 3)))).interpreter
        val query       = gqldoc("""
             {
               test
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(equalTo("""{"test":[1,2,3]}"""))
      },
      testM("ZStream used in a subscription") {
        case class Queries(test: Int)
        case class Subscriptions(test: ZStream[Any, Throwable, Int])
        val interpreter =
          graphQL(RootResolver(Queries(1), Option.empty[Unit], Subscriptions(ZStream(1, 2, 3)))).interpreter
        val query       = gqldoc("""
             subscription {
               test
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(equalTo("""{"test":<stream>}"""))
      },
      testM("Circe Json scalar") {
        import caliban.interop.circe.json._
        import io.circe.Json
        case class Queries(test: Json)

        val interpreter = graphQL(RootResolver(Queries(Json.obj(("a", Json.fromInt(333)))))).interpreter
        val query       = gqldoc("""
             {
               test
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(equalTo("""{"test":{"a":333}}"""))
      },
      testM("Play Json scalar") {
        import caliban.interop.play.json._
        import play.api.libs.json._
        case class Queries(test: JsValue)

        val interpreter = graphQL(RootResolver(Queries(Json.obj(("a", JsNumber(333)))))).interpreter
        val query       = gqldoc("""
             {
               test
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(equalTo("""{"test":{"a":333}}"""))
      },
      testM("test Interface") {
        case class Test(i: Interface)
        val interpreter = graphQL(RootResolver(Test(Interface.B("ok")))).interpreter
        val query       = gqldoc("""
             {
               i {
                 id
               }
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(equalTo("""{"i":{"id":"ok"}}"""))
      },
      testM("rename on a union child") {
        sealed trait Union
        object Union {
          case class Child(field: String) extends Union
        }
        case class Obj(union: Union)
        case class Query(test: Obj)

        object Schemas {
          implicit val schemaUnionChild: Schema[Any, Union.Child] = Schema.gen[Union.Child].rename("UnionChild")
          implicit val schemaTestUnion: Schema[Any, Union]        = Schema.gen[Union]
          implicit val schemaQuery: Schema[Any, Query]            = Schema.gen[Query]
        }
        import Schemas._

        val interpreter = graphQL(RootResolver(Query(Obj(Union.Child("f"))))).interpreter
        val query       = gqldoc("""
             {
               test {
                 union {
                   __typename
                   ... on UnionChild {
                     field
                   }
                 }
               }
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"test":{"union":{"__typename":"UnionChild","field":"f"}}}""".stripMargin)
        )
      },
      testM("rename on a union child and parent") {
        sealed trait Union
        object Union {
          case class Child(field: String) extends Union
        }
        case class Obj(union: Union)
        case class Query(test: Obj)

        object Schemas {
          implicit val schemaUnionChild: Schema[Any, Union.Child] = Schema.gen[Union.Child].rename("UnionChild")
          implicit val schemaTestUnion: Schema[Any, Union]        = Schema.gen[Union].rename("UnionRenamed")
          implicit val schemaQuery: Schema[Any, Query]            = Schema.gen[Query]
        }
        import Schemas._

        val interpreter = graphQL(RootResolver(Query(Obj(Union.Child("f"))))).interpreter
        val query       = gqldoc("""
             {
               test {
                 union {
                   __typename
                   ... on UnionChild {
                     field
                   }
                 }
               }
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"test":{"union":{"__typename":"UnionChild","field":"f"}}}""".stripMargin)
        )
      },
      testM("rename on a union child and parent") {
        sealed trait Union
        object Union {
          case class Child(field: String) extends Union
          case object ChildO              extends Union
        }
        case class Obj(union: Union)
        case class Query(test: Obj)

        object Schemas {
          implicit val schemaUnionChild: Schema[Any, Union.Child]        = Schema.gen[Union.Child].rename("UnionChild")
          implicit val schemaUnionChildO: Schema[Any, Union.ChildO.type] =
            Schema.gen[Union.ChildO.type].rename("UnionChildO")
          implicit val schemaTestUnion: Schema[Any, Union]               = Schema.gen[Union].rename("UnionRenamed")
          implicit val schemaQuery: Schema[Any, Query]                   = Schema.gen[Query]
        }
        import Schemas._

        val interpreter = graphQL(RootResolver(Query(Obj(Union.ChildO)))).interpreter
        val query       = gqldoc("""
             {
               test {
                 union {
                   __typename
                 }
               }
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"test":{"union":{"__typename":"UnionChildO"}}}""".stripMargin)
        )
      },
      testM("argument not wrapped in a case class") {
        case class Query(test: Int => Int)
        val api         = graphQL(RootResolver(Query(identity)))
        val interpreter = api.interpreter
        val query       =
          """query{
            |  test(value: 1)
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(equalTo("""{"test":1}"""))
      },
      testM("value classes") {
        case class Queries(events: List[Event], painters: List[WrappedPainter])
        val event       = Event(OrganizationId(7), "Frida Kahlo exhibition")
        val painter     = Painter("Claude Monet", "Impressionism")
        val api         = graphQL(RootResolver(Queries(event :: Nil, WrappedPainter(painter) :: Nil)))
        val interpreter = api.interpreter
        val query       =
          """query {
            |  events {
            |    organizationId
            |    title
            |  }
            |  painters {
            |    name
            |    movement
            |  }
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo(
            """{"events":[{"organizationId":7,"title":"Frida Kahlo exhibition"}],"painters":[{"name":"Claude Monet","movement":"Impressionism"}]}"""
          )
        )
      },
      testM("field name customization") {
        case class Query(@GQLName("test2") test: Int)
        val api         = graphQL(RootResolver(Query(1)))
        val interpreter = api.interpreter
        val query       =
          """query{
            |  test2
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(equalTo("""{"test2":1}"""))
      },
      testM("die bubbles to the parent") {
        case class UserArgs(id: Int)
        case class User(name: String, friends: ZIO[Any, Nothing, List[String]])
        case class Queries(user: UserArgs => ZIO[Any, Throwable, User])
        val api         = graphQL(
          RootResolver(
            Queries(args =>
              ZIO.succeed(
                User(
                  "user",
                  if (args.id == 2) ZIO.die(new Exception("Boom"))
                  else ZIO.succeed(List("friend"))
                )
              )
            )
          )
        )
        val interpreter = api.interpreter
        val query       =
          """query{
            |  user1: user(id: 1) {
            |    name
            |    friends
            |  }
            |  user2: user(id: 2) {
            |    name
            |    friends
            |  }
            |}""".stripMargin
        interpreter
          .flatMap(_.execute(query))
          .map(result =>
            assert(result.data.toString)(
              equalTo("""{"user1":{"name":"user","friends":["friend"]},"user2":null}""")
            ) &&
              assert(result.errors.collectFirst { case e: ExecutionError => e }.map(_.path))(
                isSome(equalTo(List(Left("user2"), Left("friends"))))
              )
          )
      },
      testM("die inside a nullable list") {
        case class Queries(test: List[Task[String]])
        val api         = graphQL(RootResolver(Queries(List(ZIO.succeed("a"), ZIO.die(new Exception("Boom"))))))
        val interpreter = api.interpreter
        val query       =
          """query{
            |  test
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"test":["a",null]}""")
        )
      },
      testM("die inside a non-nullable list") {
        case class Queries(test: Task[List[UIO[String]]])
        val api         = graphQL(RootResolver(Queries(Task(List(ZIO.succeed("a"), ZIO.die(new Exception("Boom")))))))
        val interpreter = api.interpreter
        val query       =
          """query{
            |  test
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"test":null}""")
        )
      },
      testM("fake field") {
        sealed trait A
        object A {
          case class B(b: Int) extends A
          case object C        extends A
        }
        case class Query(test: A)
        val interpreter = graphQL(RootResolver(Query(A.C))).interpreter
        val query = gqldoc("""
            {
              test {
                ... on C {
                  _  
                }
                ... on B {
                  b
                }
              }
            }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(equalTo("""{"test":null}"""))
      },
      testM("complex interface case") {
        @GQLInterface
        sealed trait Character {
          def id: String
          def name: String
        }
        object Character       {
          case class Human(
            id: String,
            name: String,
            height: Int
          ) extends Character

          case class Droid(
            id: String,
            name: String,
            primaryFunction: String
          ) extends Character
        }

        case class Starship(
          id: String,
          name: String,
          length: Float
        )

        // union SearchResult = Human | Droid | Starship
        type SearchResult = Either[Character, Starship]

        def eitherUnionSchema[RL, RR, L, R](name: String)(implicit
          evL: Schema[RL, L],
          evR: Schema[RR, R]
        ): Schema[RL with RR, Either[L, R]] = new Schema[RL with RR, Either[L, R]] {

          override def toType(isInput: Boolean, isSubscription: Boolean): __Type = {
            val typeL = evL.toType_(isInput = isInput, isSubscription = isSubscription)
            val typeR = evR.toType_(isInput = isInput, isSubscription = isSubscription)

            Types.makeUnion(
              name = Some(name),
              description = None,
              subTypes = typeR :: typeL.possibleTypes.getOrElse(Nil)
            )
          }

          override def resolve(value: Either[L, R]): Step[RL with RR] =
            value match {
              case Left(value)  => evL.resolve(value)
              case Right(value) => evR.resolve(value)
            }
        }

        case class SearchArgs(text: String)
        case class Query(search: SearchArgs => List[SearchResult])

        object CustomSchema {
          implicit val schemaSearchResult: Schema[Any, SearchResult] = eitherUnionSchema("SearchResult")
          implicit val schemaQuery: Schema[Any, Query]               = Schema.gen[Query]
        }

        import CustomSchema._

        val api: GraphQL[Any] =
          graphQL(
            RootResolver(
              Query(_ =>
                List(
                  Left(Character.Human("id", "name", 1)),
                  Left(Character.Droid("id", "name", "function")),
                  Right(Starship("id", "name", 3.5f))
                )
              )
            )
          )
        val interpreter       = api.interpreter
        val query             =
          """{
            |  search(text: "a") {
            |    __typename
            |    ... on Character {
            |      name
            |    }
            |    ... on Human {
            |      height
            |    }
            |    ... on Droid {
            |      primaryFunction
            |    }
            |    ... on Starship {
            |      name
            |      length
            |    }
            |  }
            |}""".stripMargin
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo(
            """{"search":[{"__typename":"Human","name":"name","height":1},{"__typename":"Droid","name":"name","primaryFunction":"function"},{"__typename":"Starship","name":"name","length":3.5}]}"""
          )
        )
      },
      testM("hand-rolled recursive schema") {
        import Schema._
        case class Group(
          id: String,
          parent: UIO[Option[Group]],
          organization: UIO[Organization]
        )
        case class Organization(id: String, groups: UIO[List[Group]])

        case class Query(
          organization: String => UIO[Organization]
        )

        implicit lazy val groupSchema: Schema[Any, Group]      = obj("Group", Some("A group of users"))(implicit ft =>
          List(
            field("id")(_.id),
            field("parent")(_.parent),
            field("organization")(_.organization)
          )
        )
        implicit lazy val orgSchema: Schema[Any, Organization] =
          obj("Organization", Some("An organization of groups"))(implicit ft =>
            List(
              field("id")(_.id),
              field("groups")(_.groups)
            )
          )

        implicit val querySchema: Schema[Any, Query] =
          obj("Query")(implicit ft =>
            List(
              fieldWithArgs[Query, String]("organization")(_.organization)
            )
          )

        val organizations: Map[String, List[String]]      = Map("abc" -> List("group1", "group2"))
        val groups: Map[String, (Option[String], String)] = Map(
          "group1" -> (None           -> "abc"),
          "group2" -> (Some("group1") -> "abc")
        )

        def getGroup(id: String): UIO[Group] = UIO(groups(id)).map { case (parent, org) =>
          Group(
            id,
            ZIO.fromOption(parent).flatMap(getGroup(_).asSomeError).optional,
            organization = getOrg(org)
          )
        }

        def getOrg(id: String) =
          UIO(organizations(id)).map(groups => Organization(id, ZIO.foreach(groups)(getGroup)))

        val interpreter = graphQL(RootResolver(Query(getOrg))).interpreter
        val query       = gqldoc("""{
             organization(value: "abc") {
               groups {
                 id
                 organization { id }
                 parent { id }
               }
             }
            }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo(
            """{"organization":{"groups":[{"id":"group1","organization":{"id":"abc"},"parent":null},{"id":"group2","organization":{"id":"abc"},"parent":{"id":"group1"}}]}}"""
          )
        )
      }
    )
}
