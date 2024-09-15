package caliban.execution

import caliban.CalibanError.ExecutionError
import caliban.InputValue.ObjectValue
import caliban.Macros.gqldoc
import caliban.TestUtils._
import caliban.Value.{ BooleanValue, IntValue, NullValue, StringValue }
import caliban._
import caliban.introspection.adt.__Type
import caliban.parsing.adt.LocationInfo
import caliban.schema.Annotations.{ GQLInterface, GQLName, GQLOneOfInput, GQLValueType }
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.auto._
import caliban.schema._
import com.github.plokhotnyuk.jsoniter_scala.core._
import zio._
import zio.stream.ZStream
import zio.test._

import java.util.UUID

object ExecutionSpec extends ZIOSpecDefault {

  @GQLInterface
  sealed trait Base {
    def id: String
    def name: String
  }
  object Base       {
    @GQLName("BaseOne")
    case class One(
      id: String,
      name: String,
      inner: List[One.Inner]
    ) extends Base
    object One {
      @GQLName("BaseOneInner")
      case class Inner(a: String)
    }

    @GQLName("BaseTwoOne")
    case class Two(
      id: String,
      name: String,
      inner: List[Two.Inner]
    ) extends Base
    object Two {
      @GQLName("BaseTwoInner")
      case class Inner(b: Int)
    }
  }

  override def spec =
    suite("ExecutionSpec")(
      test("skip directive") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
            query test {
              amos: character(name: "Amos Burton") {
                name
                nicknames @skip(if: true)
              }
            }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"amos":{"name":"Amos Burton"}}""")
        }
      },
      test("simple query with fields") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
            {
              characters {
                name
              }
            }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(
            response.data.toString ==
              """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
          )
        }
      },
      test("arguments") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
            {
              characters(origin: MARS) {
                name
                nicknames
              }
            }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(
            response.data.toString ==
              """{"characters":[{"name":"Alex Kamal","nicknames":[]},{"name":"Roberta Draper","nicknames":["Bobbie","Gunny"]}]}"""
          )
        }
      },
      test("arguments with list coercion") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
            {
              charactersIn(names: "Alex Kamal") {
                name
              }
            }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"charactersIn":[{"name":"Alex Kamal"}]}""")
        }
      },
      test("aliases") {
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

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(
            response.data.toString ==
              """{"amos":{"name":"Amos Burton","nicknames":[]},"naomi":{"name":"Naomi Nagata","nicknames":[]}}"""
          )
        }
      },
      test("effectful query") {
        val interpreter = graphQL(resolverIO).interpreter
        val query       = gqldoc("""
               {
                 characters {
                   name
                 }
               }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(
            response.data.toString ==
              """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
          )
        }
      },
      test("mutation") {
        val interpreter = graphQL(resolverWithMutation).interpreter
        val query       = gqldoc("""
               mutation {
                 deleteCharacter(name: "Amos Burton")
               }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"deleteCharacter":{}}""")
        }
      },
      test("variable") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
             query test($name: String!){
               amos: character(name: $name) {
                 name
               }
             }""")

        interpreter.flatMap(_.execute(query, None, Map("name" -> StringValue("Amos Burton")))).map { response =>
          assertTrue(response.data.toString == """{"amos":{"name":"Amos Burton"}}""")
        }
      },
      test("tolerate missing variables") {

        case class Args(term: String, id: Option[String])
        case class Test(getId: Args => Option[String])
        val api   = graphQL(RootResolver(Test(_.id)))
        val query = """query test($term: String!, $id: String) { getId(term: $term, id: $id) }"""

        api.interpreter.flatMap(_.execute(query, None, Map("term" -> StringValue("search")))).map { response =>
          val respStr = writeToString(response)
          assertTrue(respStr == """{"data":{"getId":null}}""")
        }
      },
      test("default values for variables in directives") {
        import io.circe.syntax._

        case class TestQuery(field1: String, field2: String)
        case class Query(test: TestQuery)
        val api = graphQL(RootResolver(Query(TestQuery(field1 = "1234", field2 = "5421"))))

        val query =
          """
            |query ($a: Boolean = true, $b: Boolean = false) {
            | test {
            |   field1 @include(if: $a)
            |   field2 @include(if: $b)
            | }
            |}
            |""".stripMargin

        api.interpreter.flatMap(_.execute(query, None, Map())).map { response =>
          val resp = writeToString(response)
          assertTrue(resp == """{"data":{"test":{"field1":"1234"}}}""")
        }
      },
      test("respects variables that are not provided") {
        sealed trait ThreeState
        object ThreeState {
          case object Undefined extends ThreeState
          case object Null      extends ThreeState
          case object Value     extends ThreeState

          def fromOption[T](o: Option[T]) = o.fold[ThreeState](Null)(_ => Value)
        }

        implicit val argBuilder: ArgBuilder[ThreeState] = new ArgBuilder[ThreeState] {
          private val base = ArgBuilder.option(ArgBuilder.boolean)

          override def build(input: InputValue) = base.build(input).map(ThreeState.fromOption(_))

          override def buildMissing(default: Option[String]) = default match {
            case None    => Right(ThreeState.Undefined)
            case Some(v) => base.buildMissing(Some(v)).map(ThreeState.fromOption(_))
          }
        }

        implicit val schema: Schema[Any, ThreeState] = Schema.optionSchema(Schema.booleanSchema).contramap {
          case ThreeState.Undefined => None
          case ThreeState.Null      => Some(false)
          case ThreeState.Value     => Some(true)
        }

        case class Args(term: String, state: ThreeState)
        case class Test(getState: Args => ThreeState)
        val api          = graphQL(RootResolver(Test(_.state)))
        val query        = """query test($term: String!, $state: Boolean) { getState(term: $term, state: $state) }"""
        val queryDefault =
          """query test($term: String!, $state: Boolean = null) { getState(term: $term, state: $state) }"""

        def execute(query: String, state: ThreeState) = {
          val vars = Map(
            "term"  -> Some(StringValue("search")),
            "state" -> (state match {
              case ThreeState.Undefined => None
              case ThreeState.Null      => Some(NullValue)
              case ThreeState.Value     => Some(BooleanValue(false))
            })
          ).collect { case (k, Some(v)) => k -> v }
          api.interpreter.flatMap(_.execute(query, None, vars))
        }

        for {
          undefined    <- execute(query, ThreeState.Undefined)
          nul          <- execute(query, ThreeState.Null)
          value        <- execute(query, ThreeState.Value)
          default      <- execute(queryDefault, ThreeState.Undefined)
          defaultValue <- execute(queryDefault, ThreeState.Value)
        } yield assertTrue(undefined.data.toString == """{"getState":null}""") &&
          assertTrue(nul.data.toString == """{"getState":false}""") &&
          assertTrue(value.data.toString == """{"getState":true}""") &&
          assertTrue(default.data.toString == """{"getState":false}""") &&
          assertTrue(defaultValue.data.toString == """{"getState":true}""")
      },
      test("field function") {
        import io.circe.syntax._

        case class Character(name: String = "Bob")
        case class Test(character: Field => Character)
        val api   = graphQL(RootResolver(Test(_ => Character())))
        val query = """query test { character { name } }"""

        api.interpreter.flatMap(_.execute(query, None, Map())).map { response =>
          val resp = writeToString(response)
          assertTrue(resp == """{"data":{"character":{"name":"Bob"}}}""")
        }
      },
      test("field function with input") {
        import io.circe.syntax._

        case class NameInput(name: String)
        case class Character(name: String)
        case class Test(character: Field => NameInput => Character)
        val api   = graphQL(RootResolver(Test(_ => input => Character(input.name))))
        val query = """query test { character(name: "Bob") { name }}"""

        api.interpreter.flatMap(_.execute(query, None, Map())).map { response =>
          val resp = writeToString(response)
          assertTrue(resp == """{"data":{"character":{"name":"Bob"}}}""")
        }
      },
      test("""input can contain field named "value"""") {
        import io.circe.syntax._
        case class NonNegInt(value: Int)
        case class Args(int: NonNegInt, value: String)
        case class Test(q: Args => Unit)

        implicit val nonNegIntArgBuilder: ArgBuilder[NonNegInt] = ArgBuilder.int.flatMap {
          case i if i > 0 => Right(NonNegInt(i))
          case neg        => Left(CalibanError.ExecutionError(s"$neg is negative"))
        }
        implicit val nonNegIntSchema: Schema[Any, NonNegInt]    = Schema.intSchema.contramap(_.value)

        val api   = graphQL(RootResolver(Test(_ => ())))
        val query = """query {q(int: -1, value: "value")}"""
        api.interpreter
          .flatMap(_.execute(query, None, Map("int" -> IntValue(-1), "value" -> StringValue("str value"))))
          .map { response =>
            val resp = writeToString(response)
            assertTrue(
              resp ==
                """{"data":null,"errors":[{"message":"-1 is negative","locations":[{"line":1,"column":8}],"path":["q"]}]}"""
            )
          }
      },
      test("variable in list") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
             query test($name: String) {
               amos: charactersIn(names: [$name]){
                 name
               }
              }""")

        interpreter.flatMap(_.execute(query, None, Map("name" -> StringValue("Amos Burton")))).map { response =>
          assertTrue(response.data.toString == """{"amos":[{"name":"Amos Burton"}]}""")
        }
      },
      test("variable in object") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
             query test($name: String!) {
               exists(character: { name: $name, nicknames: [], origin: EARTH })
              }""")

        interpreter.flatMap(_.execute(query, None, Map("name" -> StringValue("Amos Burton")))).map { response =>
          assertTrue(response.data.toString == """{"exists":true}""")
        }
      },
      test("skip directive") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
             query test{
               amos: character(name: "Amos Burton") {
                 name
                 nicknames @skip(if: true)
               }
             }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"amos":{"name":"Amos Burton"}}""")
        }
      },
      test("include directive") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
             query test($included: Boolean!){
               amos: character(name: "Amos Burton") {
                 name
                 nicknames @include(if: $included)
               }
             }""")

        interpreter.flatMap(_.execute(query, None, Map("included" -> BooleanValue(false)))).map { response =>
          assertTrue(response.data.toString == """{"amos":{"name":"Amos Burton"}}""")
        }
      },
      test("test Map") {
        case class Test(map: Map[Int, String])
        val interpreter = graphQL(RootResolver(Test(Map(3 -> "ok")))).interpreter
        val query       = gqldoc("""
             {
               map {
                 key
                 value
               }
             }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"map":[{"key":3,"value":"ok"}]}""")
        }
      },
      test("test Either") {
        case class Test(either: Either[Int, String])
        val interpreter = graphQL(RootResolver(Test(Right("ok")))).interpreter
        val query       = gqldoc("""
             {
               either {
                 left
                 right
               }
             }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"either":{"left":null,"right":"ok"}}""")
        }
      },
      test("test UUID") {
        case class IdArgs(id: UUID)
        case class Queries(test: IdArgs => UUID)
        val interpreter = graphQL(RootResolver(Queries(_.id))).interpreter
        val query       = gqldoc("""
             {
               test(id: "be722453-d97d-48c2-b535-9badd1b5d4c9")
             }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":"be722453-d97d-48c2-b535-9badd1b5d4c9"}""")
        }
      },
      test("mapError") {
        import io.circe.syntax._
        case class Test(either: Either[Int, String])
        val api   = graphQL(RootResolver(Test(Right("ok"))))
        val query = """query{}"""

        for {
          interpreter <- api.interpreter
          result      <- interpreter.mapError(_ => "my custom error").execute(query)
          resp         = writeToString(result)
        } yield assertTrue(result.errors == List("my custom error")) &&
          assertTrue(resp == """{"data":null,"errors":[{"message":"my custom error"}]}""")
      },
      test("customErrorEffectSchema") {
        import io.circe.syntax._
        case class Test(test: IO[Int, String])

        implicit def customEffectSchema[A](implicit s: Schema[Any, A]): Schema[Any, IO[Int, A]] =
          Schema.customErrorEffectSchema((i: Int) => ExecutionError(s"my custom error $i"))

        val api   = graphQL(RootResolver(Test(ZIO.fail(1))))
        val query = """query { test }"""

        for {
          interpreter <- api.interpreter
          result      <- interpreter.execute(query)
          resp         = writeToString(result)
        } yield assertTrue(
          resp == """{"data":{"test":null},"errors":[{"message":"my custom error 1","locations":[{"line":1,"column":9}],"path":["test"]}]}"""
        )
      },
      test("merge 2 APIs") {
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
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"name":"name","id":2}""")
        }
      },
      test("error path") {
        case class A(b: B)
        case class B(c: IO[Throwable, Int])
        case class Test(a: A)
        val e           = new Exception("boom")
        val interpreter = graphQL(RootResolver(Test(A(B(ZIO.fail(e)))))).interpreter
        val query       = gqldoc("""
              {
                a {
                  b {
                    c
                  }
                }
              }""")
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(
            response.errors == List(
              ExecutionError(
                "Effect failure",
                List(StringValue("a"), StringValue("b"), StringValue("c")),
                Some(LocationInfo(21, 5)),
                Some(e)
              )
            )
          )
        }
      },
      test("ZStream used in a query") {
        case class Queries(test: ZStream[Any, Throwable, Int])
        val interpreter = graphQL(RootResolver(Queries(ZStream(1, 2, 3)))).interpreter
        val query       = gqldoc("""
             {
               test
             }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":[1,2,3]}""")
        }
      },
      test("ZStream used in a subscription") {
        case class Queries(test: Int)
        case class Subscriptions(test: ZStream[Any, Throwable, Int])
        val interpreter =
          graphQL(RootResolver(Some(Queries(1)), Option.empty[Unit], Some(Subscriptions(ZStream(1, 2, 3))))).interpreter
        val query       = gqldoc("""
             subscription {
               test
             }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":<stream>}""")
        }
      },
      test("ARGS => ZStream used in a subscription") {
        case class Queries(test: Int)
        case class Subscriptions(test: Int => ZStream[Any, Throwable, Int])
        val interpreter =
          graphQL(
            RootResolver(Some(Queries(1)), Option.empty[Unit], Some(Subscriptions(_ => ZStream(1, 2, 3))))
          ).interpreter
        val query       = gqldoc("""
             subscription {
               test(value: 1)
             }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":<stream>}""")
        }
      },
      test("ARGS => ZStream used in a subscription with context") {
        // setup up an authentication FiberRev Environment
        case class Req(id: Int)
        case class AuthToken(value: String)
        type Auth = FiberRef[Option[AuthToken]]

        val authLayer: ZLayer[Any, Nothing, FiberRef[Option[AuthToken]]] = ZLayer.scoped {
          FiberRef.make(Option.empty[AuthToken])
        }

        // set up the graphql resolvers
        case class Queries(test: Int)
        case class Subscriptions(test: Req => ZStream[Auth, Throwable, String])

        // create a custom schema for the Auth Env
        object schema extends GenericSchema[Auth]
        import schema.auto._

        // effectfully produce a stream using the environment
        def getStream(req: Req) = ZStream.fromZIO(for {
          tokenRef <- ZIO.service[FiberRef[Option[AuthToken]]]
          tokenOpt <- tokenRef.get
        } yield tokenOpt.map(_.value).getOrElse("NONE"))

        // set up a wrapped interpreter, setting the authentication token in the auth context
        val interpreter        =
          graphQL[Auth, Queries, Unit, Subscriptions](
            RootResolver(
              queryResolver = Some(Queries(1)),
              mutationResolver = Option.empty[Unit],
              subscriptionResolver = Some(Subscriptions(getStream))
            )
          ).interpreter
        val wrappedInterpreter = for {
          auth <- ZIO.service[FiberRef[Option[AuthToken]]]
          _    <- auth.set(Some(AuthToken("TOKEN")))
          i    <- interpreter
        } yield i

        // run the query and materialize the result to ensure it has what we expect
        val query = gqldoc("""
             subscription {
               test(id: 1)
             }""")

        def exec = wrappedInterpreter
          .flatMap(_.execute(query))
          .map(_.data)
          .flatMap {
            case ResponseValue.ObjectValue(fields) =>
              fields.head match {
                case ("test", ResponseValue.StreamValue(stream)) =>
                  stream.runHead.someOrFailException.map {
                    case Value.StringValue(s) => s
                    case x                    => s"Non-string stream value $x"
                  }
                case x                                           => ZIO.succeed(s"No test stream value found in $x")
              }
            case x                                 => ZIO.succeed(s"No stream found in ${x.getClass}")
          }
          .provideSomeLayer(authLayer)

        exec.map(str => assertTrue(str == "TOKEN"))
      },
      test("Circe Json scalar") {
        import caliban.interop.circe.json._
        import io.circe.Json
        case class Queries(test: Json)

        val interpreter = graphQL(RootResolver(Queries(Json.obj(("a", Json.fromInt(333)))))).interpreter
        val query       = gqldoc("""
             {
               test
             }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":{"a":333}}""")
        }
      },
      test("Play Json scalar") {
        import caliban.interop.play.json._
        import play.api.libs.json._
        case class Queries(test: JsValue)

        val interpreter = graphQL(RootResolver(Queries(Json.obj(("a", JsNumber(333)))))).interpreter
        val query       = gqldoc("""
             {
               test
             }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":{"a":333}}""")
        }
      },
      test("zio-json scalar") {
        import zio.json.ast._
        import caliban.interop.zio.json._

        case class JsonArg(value: Json)
        case class Queries(test: JsonArg => Json)

        val interpreter = graphQL(
          RootResolver(
            Queries(json =>
              Json.Obj(
                "a" -> Json.Arr(
                  Json.Num(333),
                  Json.Str("hello"),
                  Json.Bool(false),
                  Json.Null,
                  Json.Obj("b" -> Json.Num(444))
                )
              ) merge json.value
            )
          )
        ).interpreter
        val query       = gqldoc("""
             {
               test(value: {world: 123})
             }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":{"a":[333,"hello",false,null,{"b":444}],"world":123}}""")
        }
      },
      test("test Interface") {
        case class Test(i: Interface)
        val interpreter = graphQL(RootResolver(Test(Interface.B("ok")))).interpreter
        val query       = gqldoc("""
             {
               i {
                 id
               }
             }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"i":{"id":"ok"}}""")
        }
      },
      test("rename on a union child") {
        sealed trait Union
        object Union {
          case class Child(field: String) extends Union
        }
        case class Obj(union: Union)
        case class Query(test: Obj)

        object Schemas {
          implicit val schemaUnionChild: Schema[Any, Union.Child] =
            genAll[Any, Union.Child].rename("UnionChild")
          implicit val schemaTestUnion: Schema[Any, Union]        = genAll
          implicit val schemaQuery: Schema[Any, Query]            = genAll
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

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":{"union":{"__typename":"UnionChild","field":"f"}}}""")
        }
      },
      test("rename on a union child and parent") {
        sealed trait Union
        object Union {
          case class Child(field: String) extends Union
        }
        case class Obj(union: Union)
        case class Query(test: Obj)

        object Schemas {
          implicit val schemaUnionChild: Schema[Any, Union.Child] =
            genAll[Any, Union.Child].rename("UnionChild")
          implicit val schemaTestUnion: Schema[Any, Union]        = genAll[Any, Union].rename("UnionRenamed")
          implicit val schemaQuery: Schema[Any, Query]            = genAll
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

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":{"union":{"__typename":"UnionChild","field":"f"}}}""")
        }
      },
      test("rename on a union child and parent (typename)") {
        sealed trait Union
        object Union {
          case class Child(field: String) extends Union
          case object ChildO              extends Union
        }
        case class Obj(union: Union)
        case class Query(test: Obj)

        object Schemas {
          implicit val schemaUnionChild: Schema[Any, Union.Child]        =
            genAll[Any, Union.Child].rename("UnionChild")
          implicit val schemaUnionChildO: Schema[Any, Union.ChildO.type] =
            Schema.gen[Any, Union.ChildO.type].rename("UnionChildO")
          implicit val schemaTestUnion: Schema[Any, Union]               = genAll[Any, Union].rename("UnionRenamed")
          implicit val schemaQuery: Schema[Any, Query]                   = genAll
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

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":{"union":{"__typename":"UnionChildO"}}}""")
        }
      },
      test("argument not wrapped in a case class") {
        case class Query(test: Int => Int)
        val api         = graphQL(RootResolver(Query(identity)))
        val interpreter = api.interpreter
        val query       =
          """query{
            |  test(value: 1)
            |}""".stripMargin
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":1}""")
        }
      },
      test("field name customization") {
        case class Query(@GQLName("test2") test: Int)
        val api         = graphQL(RootResolver(Query(1)))
        val interpreter = api.interpreter
        val query       =
          """query{
            |  test2
            |}""".stripMargin
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test2":1}""")
        }
      },
      test("die bubbles to the parent") {
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
            assertTrue(result.data.toString == """{"user1":{"name":"user","friends":["friend"]},"user2":null}""") &&
              assertTrue(
                result.errors.collectFirst { case e: ExecutionError => e }.map(_.path).get ==
                  List(StringValue("user2"), StringValue("friends"))
              )
          )
      },
      test("failure in ArgBuilder, optional field") {
        trait A
        case class UserArgs(id: A)
        case class User(test: UserArgs => String)
        case class Mutations(user: Task[User])
        case class Queries(a: Int)
        implicit val aArgBuilder: ArgBuilder[A] = (_: InputValue) => Left(ExecutionError("nope"))
        implicit val aSchema: Schema[Any, A]    = Schema.scalarSchema("a", None, None, None, _ => IntValue(1))

        val api = graphQL(RootResolver(Queries(1), Mutations(ZIO.succeed(User(_.toString)))))

        val interpreter = api.interpreter
        val query       =
          """mutation {
            |  user {
            |    test(id: 1)
            |  }
            |}""".stripMargin
        interpreter
          .flatMap(_.execute(query))
          .map(result => assertTrue(result.data.toString == """{"user":null}"""))
      },
      test("failure in ArgBuilder, non optional field") {
        case class UserArgs(id: Int)
        case class User(test: UserArgs => String)
        case class Mutations(user: UIO[User])
        case class Queries(a: Int)
        val api = graphQL(RootResolver(Queries(1), Mutations(ZIO.succeed(User(_.toString)))))

        val interpreter = api.interpreter
        val query       =
          """mutation {
            |  user {
            |    test(id: "wrong")
            |  }
            |}""".stripMargin
        interpreter
          .flatMap(_.execute(query))
          .map(result => assertTrue(result.data.toString == """null"""))
      },
      test("using value types as inputs") {
        @GQLValueType
        case class UserArgs(id: Int)
        case class User(test: UserArgs => String)
        case class Mutations(user: UIO[User])
        case class Queries(a: Int)
        val api = graphQL(RootResolver(Queries(1), Mutations(ZIO.succeed(User(_.toString)))))

        val interpreter = api.interpreter
        val query       =
          """mutation {
            |  user {
            |    test(value: 123)
            |  }
            |}""".stripMargin
        interpreter
          .flatMap(_.execute(query))
          .debug
          .map(_.data.toString)
          .map(result => assertTrue(result == """{"user":{"test":"UserArgs(123)"}}"""))
      },
      test("die inside a nullable list") {
        case class Queries(test: List[Task[String]])
        val api         = graphQL(RootResolver(Queries(List(ZIO.succeed("a"), ZIO.die(new Exception("Boom"))))))
        val interpreter = api.interpreter
        val query       =
          """query{
            |  test
            |}""".stripMargin
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":["a",null]}""")
        }
      },
      test("die inside a non-nullable list") {
        case class Queries(test: Task[List[UIO[String]]])
        val api         = graphQL(RootResolver(Queries(ZIO.succeed(List(ZIO.succeed("a"), ZIO.die(new Exception("Boom")))))))
        val interpreter = api.interpreter
        val query       =
          """query{
            |  test
            |}""".stripMargin
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":null}""")
        }
      },
      test("fake field") {
        sealed trait A
        object A {
          case class B(b: Int) extends A
          case object C        extends A
        }
        case class Query(test: A)
        implicit val schemaB: Schema[Any, A.B] = Schema.gen
        implicit val schemaC: Schema[Any, A.C.type]          = genAll
        implicit val schemaCharacter: Schema[Any, Character] = genAll
        val interpreter                                      = graphQL(RootResolver(Query(A.C))).interpreter
        val query                                            = gqldoc("""
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

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":null}""")
        }
      },
      test("complex interface case") {
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
          implicit val schemaHuman: Schema[Any, Character.Human]     = Schema.gen
          implicit val schemaDroid: Schema[Any, Character.Droid]     = Schema.gen
          implicit val schemaSearchResult: Schema[Any, SearchResult] = eitherUnionSchema("SearchResult")
          implicit val schemaQuery: Schema[Any, Query]               = Schema.gen
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
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(
            response.data.toString ==
              """{"search":[{"__typename":"Human","name":"name","height":1},{"__typename":"Droid","name":"name","primaryFunction":"function"},{"__typename":"Starship","name":"name","length":3.5}]}"""
          )
        }
      },
      test("hand-rolled recursive schema") {
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

        def getGroup(id: String): UIO[Group] = ZIO.succeed(groups(id)).map { case (parent, org) =>
          Group(
            id,
            ZIO.fromOption(parent).flatMap(getGroup(_).asSomeError).unsome,
            organization = getOrg(org)
          )
        }

        def getOrg(id: String) =
          ZIO.succeed(organizations(id)).map(groups => Organization(id, ZIO.foreach(groups)(getGroup)))

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

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(
            response.data.toString ==
              """{"organization":{"groups":[{"id":"group1","organization":{"id":"abc"},"parent":null},{"id":"group2","organization":{"id":"abc"},"parent":{"id":"group1"}}]}}"""
          )
        }
      },
      test("hand-rolled recursive lazy schema") {
        import Schema._
        case class Foo(id: Int) {
          def bar(): Bar = Bar(234)
        }

        case class Bar(id: Int) {
          def foo(): Foo = Foo(123)
        }

        implicit lazy val fooSchema: Schema[Any, Foo] = obj("Foo", None)(implicit ft =>
          List(
            field("id")(_.id),
            fieldLazy("bar")(_.bar())
          )
        )

        implicit lazy val barSchema: Schema[Any, Bar] = obj("Bar", None)(implicit ft =>
          List(
            field("id")(_.id),
            fieldLazy("foo")(_.foo())
          )
        )

        case class Queries(foos: Seq[Foo])

        val queries: Queries = Queries(Seq(Foo(123)))

        val calibanApi: GraphQL[Any] = graphQL(RootResolver(queries))

        val interpreter = calibanApi.interpreter
        val query       = gqldoc("""{
            foos {
              id
              bar {
                id
              }
            }
          }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"foos":[{"id":123,"bar":{"id":234}}]}""")
        }
      },
      test("directives on hand-rolled schema") {
        import Schema._
        import caliban.parsing.adt.Directive

        case class Foo(fieldA: String => String = _ => "foo", fieldB: String = "foo")

        implicit lazy val fooSchema: Schema[Any, Foo] = obj("Foo", None)(implicit ft =>
          List(
            fieldWithArgs(
              "fieldA",
              Some("Description"),
              List(
                Directive(
                  "deprecated",
                  Map(
                    "reason" -> Value.StringValue("due to reasons")
                  )
                )
              )
            )(_.fieldA),
            field(
              "fieldB",
              Some("Description"),
              List(
                Directive(
                  "deprecated",
                  Map(
                    "reason" -> Value.StringValue("due to reasons")
                  )
                )
              )
            )(_.fieldB)
          )
        )

        case class Queries(foo: Foo)

        val queries: Queries = Queries(Foo())

        val api: GraphQL[Any] = graphQL(RootResolver(queries))
        val interpreter       = api.interpreter

        val query = gqldoc("""{
            __type(name: "Foo") {
              name
              fields(includeDeprecated: true) {
                name
                isDeprecated
                deprecationReason
              }
            }
          }""")

        val expected =
          """{"__type":{"name":"Foo","fields":[{"name":"fieldA","isDeprecated":true,"deprecationReason":"due to reasons"},{"name":"fieldB","isDeprecated":true,"deprecationReason":"due to reasons"}]}}"""

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == expected)
        }
      },
      test("union redirect") {
        sealed trait Foo

        case class Bar(int: Int, common: Boolean) extends Foo

        case class Baz(value: String, common: Boolean)

        @GQLValueType
        case class Redirect(baz: Baz) extends Foo

        case class Queries(foos: List[Foo])

        val queries = Queries(
          List(
            Bar(42, common = true),
            Redirect(Baz("hello", common = false))
          )
        )

        val api: GraphQL[Any] = graphQL(RootResolver(queries))
        val interpreter       = api.interpreter
        val query             = gqldoc("""{
            foos {
              ... on Bar { common int }
              ... on Baz { common value }
            }
            }""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(
            response.data.toString == """{"foos":[{"common":true,"int":42},{"common":false,"value":"hello"}]}"""
          )
        }
      },
      test("value type not scalar") {
        @GQLValueType
        case class Wrapper(value: Int)
        case class Queries(test: Wrapper)

        val queries = Queries(Wrapper(2))

        val api: GraphQL[Any] = graphQL(RootResolver(queries))
        val interpreter       = api.interpreter
        val query             = gqldoc("""{test}""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":2}""")
        }
      },
      test("value type scalar") {
        @GQLValueType(isScalar = true)
        case class Wrapper(value: Int)
        case class Queries(test: Wrapper)

        val queries = Queries(Wrapper(2))

        val api: GraphQL[Any] = graphQL(RootResolver(queries))
        val interpreter       = api.interpreter
        val query             = gqldoc("""{test}""")

        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """{"test":2}""")
        }
      },
      test("conflicting fragments selection merging") {

        val base1 = Base.One(
          id = "1",
          name = "base 1",
          inner = List(Base.One.Inner(a = "a"))
        )
        val base2 = Base.Two(
          id = "2",
          name = "base 2",
          inner = List(Base.Two.Inner(b = 2))
        )
        case class Test(bases: List[Base])

        implicit val baseSchema: Schema[Any, Base] = Schema.gen

        val api   = graphQL(RootResolver(Test(List(base1, base2))))
        val query = """
        query {
          bases {
            id
            ... on BaseOne {
              id
              name
              inner { a }
            }
            ... on BaseTwoOne {
              id
              name
              inner { b }
            }
          }
        }
      """

        api.interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(
            response.data.toString == """{"bases":[{"id":"1","name":"base 1","inner":[{"a":"a"}]},{"id":"2","name":"base 2","inner":[{"b":2}]}]}"""
          )
        }
      },
      test("custom enum schemas") {
        trait MyEnum
        object MyEnum {
          case object A extends MyEnum
          case object B extends MyEnum
        }
        implicit val enumSchema: Schema[Any, MyEnum] =
          Schema.enumSchema("Foo", values = List(enumValue("A")), repr = _.toString)

        case class Query(valid: MyEnum, invalid: MyEnum)

        val api: GraphQL[Any] = graphQL(RootResolver(Query(MyEnum.A, MyEnum.B)))

        for {
          interpreter <- api.interpreter
          valid       <- interpreter.execute(gqldoc("""{ valid }"""))
          invalid     <- interpreter.execute(gqldoc("""{ invalid }"""))
        } yield assertTrue(
          valid.data.toString == """{"valid":"A"}""",
          invalid.data.toString == "null",
          invalid.errors.nonEmpty
        )
      },
      test("defects set the first nullable parent value to null") {
        case class Bar(value: UIO[String])
        case class Foo1(bar: Option[Bar], value: String)
        case class Query1(foo: Foo1)

        case class Foo2(bar: Bar, value: String)
        case class Query2(foo: Option[Foo2])

        val boom               = ZIO.die(ExecutionError("boom"))
        val api1: GraphQL[Any] = graphQL(RootResolver(Query1(Foo1(Some(Bar(boom)), "foo"))))
        val api2: GraphQL[Any] = graphQL(RootResolver(Query2(Some(Foo2(Bar(boom), "foo")))))

        val query = gqldoc("""{ foo { value bar { value } } }""")
        for {
          interpreter1 <- api1.interpreter
          result1      <- interpreter1.execute(query)
          data1         = result1.data.toString
          errors1       = result1.errors.map(_.toResponseValue.toString)
          interpreter2 <- api2.interpreter
          result2      <- interpreter2.execute(query)
          data2         = result2.data.toString
          errors2       = result2.errors.map(_.toResponseValue.toString)
        } yield assertTrue(
          data1 == """{"foo":{"value":"foo","bar":null}}""",
          errors1 == List(
            """{"message":"boom","locations":[{"line":1,"column":21}],"path":["foo","bar","value"]}"""
          ),
          data2 == """{"foo":null}""",
          errors2 == List(
            """{"message":"boom","locations":[{"line":1,"column":21}],"path":["foo","bar","value"]}"""
          )
        )

      },
      test("exceptions from function set the first nullable parent value to null") {
        case class Bar(value: () => String)
        case class Foo1(bar: Option[Bar], value: String)
        case class Query1(foo: Foo1)

        case class Foo2(bar: Bar, value: String)
        case class Query2(foo: Option[Foo2])

        val boom               = () => throw ExecutionError("boom")
        val api1: GraphQL[Any] = graphQL(RootResolver(Query1(Foo1(Some(Bar(boom)), "foo"))))
        val api2: GraphQL[Any] = graphQL(RootResolver(Query2(Some(Foo2(Bar(boom), "foo")))))

        val query = gqldoc("""{ foo { value bar { value } } }""")
        for {
          interpreter1 <- api1.interpreter
          result1      <- interpreter1.execute(query)
          data1         = result1.data.toString
          errors1       = result1.errors.map(_.toResponseValue.toString)
          interpreter2 <- api2.interpreter
          result2      <- interpreter2.execute(query)
          data2         = result2.data.toString
          errors2       = result2.errors.map(_.toResponseValue.toString)
        } yield assertTrue(
          data1 == """{"foo":{"value":"foo","bar":null}}""",
          errors1 == List(
            """{"message":"boom","locations":[{"line":1,"column":21}],"path":["foo","bar","value"]}"""
          ),
          data2 == """{"foo":null}""",
          errors2 == List(
            """{"message":"boom","locations":[{"line":1,"column":21}],"path":["foo","bar","value"]}"""
          )
        )

      },
      test("top-level fields are executed sequentially for mutations") {
        case class Foo(field1: UIO[Unit], field2: UIO[Unit])
        case class Mutations(
          mutation1: CharacterArgs => UIO[Foo],
          mutation2: CharacterArgs => UIO[Foo]
        )

        val ref                                       = Unsafe.unsafe(implicit u => Ref.unsafe.make(List.empty[String]))
        def add(name: String, d: Duration = 1.second) = ref.update(name :: _).delay(d)
        def foo(prefix: String)                       = ZIO.succeed(Foo(add(s"$prefix-f1", 1500.millis), add(s"$prefix-f2", 2.seconds)))

        val interpreter = graphQL(
          RootResolver(
            resolverIO.queryResolver,
            Mutations(
              _ => add("m1") *> foo("m1"),
              _ => add("m2") *> foo("m2")
            )
          )
        ).interpreter

        def adjustAndGet(d: Duration = 1.second) = TestClock.adjust(d) *> ref.get

        for {
          i  <- interpreter
          _  <- i.execute(gqldoc("""mutation {
                mutation1(name: "foo") { field1 field2 }
                mutation2(name: "bar") { field1 field2 }
              }"""))
                  .fork
          r1 <- ref.get
          r2 <- adjustAndGet()
          r3 <- adjustAndGet()
          r4 <- adjustAndGet()
          r5 <- adjustAndGet()
          r6 <- adjustAndGet()
          r7 <- adjustAndGet()
        } yield assertTrue(
          r1 == Nil,
          r2 == List("m1"),
          r3 == List("m1"),
          r4 == List("m1-f2", "m1-f1", "m1"),
          r5 == List("m2", "m1-f2", "m1-f1", "m1"),
          r6 == List("m2", "m1-f2", "m1-f1", "m1"),
          r7 == List("m2-f2", "m2-f1", "m2", "m1-f2", "m1-f1", "m1")
        )
      },
      test("oneOf inputs") {

        @GQLOneOfInput
        sealed trait Foo
        object Foo {
          case class FooString(stringValue: String) extends Foo
          case class FooInt(intValue: Int)          extends Foo

          case class Wrapper(fooInput: Foo)
        }

        case class Queries(foo: Foo.Wrapper => String, fooUnwrapped: Foo => String)

        implicit val fooStringAb: ArgBuilder[Foo.FooString] = ArgBuilder.gen
        implicit val fooIntAb: ArgBuilder[Foo.FooInt]       = ArgBuilder.gen
        implicit val fooAb: ArgBuilder[Foo]                 = ArgBuilder.gen
        implicit val schema: Schema[Any, Queries]           = Schema.gen

        val api: GraphQL[Any] = graphQL(
          RootResolver(
            Queries(
              {
                case Foo.Wrapper(Foo.FooString(value)) => value
                case Foo.Wrapper(Foo.FooInt(value))    => value.toString
              },
              {
                case Foo.FooString(value) => value
                case Foo.FooInt(value)    => value.toString
              }
            )
          )
        )

        val cases = List(
          gqldoc("""{ foo(fooInput: {stringValue: "hello"}) }""")                 -> """{"foo":"hello"}""",
          gqldoc("""{ foo(fooInput: {intValue: 42}) }""")                         -> """{"foo":"42"}""",
          gqldoc("""{ fooUnwrapped(value: {intValue: 42}) }""")                   -> """{"fooUnwrapped":"42"}""",
          gqldoc("""query Foo($args: FooInput!){ fooUnwrapped(value: $args) }""") -> """{"fooUnwrapped":"42"}"""
        )

        ZIO.foldLeft(cases)(assertCompletes) { case (acc, (query, expected)) =>
          api.interpreter
            .flatMap(_.execute(query, variables = Map("args" -> ObjectValue(Map("intValue" -> IntValue(42))))))
            .map(response => assertTrue(response.data.toString == expected))
        }
      }
    )
}
