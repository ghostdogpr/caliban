package caliban.wrappers

import caliban.CalibanError.{ ExecutionError, ValidationError }
import caliban.GraphQL._
import caliban.InputValue.ObjectValue
import caliban.Macros.gqldoc
import caliban.TestUtils.resolver
import caliban.Value.StringValue
import caliban.introspection.adt.{ __Directive, __DirectiveLocation }
import caliban.schema.Annotations.GQLDirective
import caliban.schema.GenericSchema
import caliban.wrappers.ApolloCaching.CacheControl
import caliban.wrappers.ApolloPersistedQueries.apolloPersistedQueries
import caliban.wrappers.Wrapper.{ ExecutionWrapper, FieldWrapper }
import caliban.wrappers.Wrappers._
import caliban._
import io.circe.syntax._
import zio.clock.Clock
import zio.duration._
import zio.query.ZQuery
import zio.test.Assertion._
import zio.test._
import zio.test.environment.{ TestClock, TestEnvironment }
import zio.{ clock, Promise, Ref, UIO, URIO, ZIO }

import scala.language.postfixOps

object WrappersSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("WrappersSpec")(
      testM("wrapPureValues false") {
        case class Test(a: Int, b: UIO[Int])
        for {
          ref         <- Ref.make[Int](0)
          wrapper      = FieldWrapper[Any](
                           { case (query, _) => ZQuery.fromEffect(ref.update(_ + 1)) *> query },
                           wrapPureValues = false
                         )
          interpreter <- (graphQL(RootResolver(Test(1, UIO(2)))) @@ wrapper).interpreter.orDie
          query        = gqldoc("""{ a b }""")
          _           <- interpreter.execute(query)
          counter     <- ref.get
        } yield assert(counter)(equalTo(1))
      },
      testM("wrapPureValues true") {
        case class Test(a: Int, b: UIO[Int])
        for {
          ref         <- Ref.make[Int](0)
          wrapper      = FieldWrapper[Any](
                           { case (query, _) => ZQuery.fromEffect(ref.update(_ + 1)) *> query },
                           wrapPureValues = true
                         )
          interpreter <- (graphQL(RootResolver(Test(1, UIO(2)))) @@ wrapper).interpreter.orDie
          query        = gqldoc("""{ a b }""")
          _           <- interpreter.execute(query)
          counter     <- ref.get
        } yield assert(counter)(equalTo(2))
      },
      testM("Max fields") {
        case class A(b: B)
        case class B(c: Int)
        case class Test(a: A)
        val interpreter = (graphQL(RootResolver(Test(A(B(2))))) @@ maxFields(2)).interpreter
        val query       = gqldoc("""
              {
                a {
                  b {
                    c
                  }
                }
              }""")
        assertM(interpreter.flatMap(_.execute(query)).map(_.errors))(
          equalTo(List(ValidationError("Query has too many fields: 3. Max fields: 2.", "")))
        )
      },
      testM("Max fields with fragment") {
        case class A(b: B)
        case class B(c: Int)
        case class Test(a: A)
        val interpreter = (graphQL(RootResolver(Test(A(B(2))))) @@ maxFields(2)).interpreter
        val query       = gqldoc("""
              query test {
                a {
                  ...f
                }
              }
              
              fragment f on A {
                b {
                  c 
                }
              }
              """)
        assertM(interpreter.flatMap(_.execute(query)).map(_.errors))(
          equalTo(List(ValidationError("Query has too many fields: 3. Max fields: 2.", "")))
        )
      },
      testM("Max depth") {
        case class A(b: B)
        case class B(c: Int)
        case class Test(a: A)
        val interpreter = (graphQL(RootResolver(Test(A(B(2))))) @@ maxDepth(2)).interpreter
        val query       = gqldoc("""
              {
                a {
                  b {
                    c
                  }
                }
              }""")
        assertM(interpreter.flatMap(_.execute(query)).map(_.errors))(
          equalTo(List(ValidationError("Query is too deep: 3. Max depth: 2.", "")))
        )
      },
      testM("Timeout") {
        case class Test(a: URIO[Clock, Int])

        object schema extends GenericSchema[Clock]
        import schema._

        val interpreter =
          (graphQL(RootResolver(Test(clock.sleep(2 minutes).as(0)))) @@ timeout(1 minute)).interpreter
        val query       = gqldoc("""
              {
                a
              }""")
        assertM(for {
          fiber <- interpreter.flatMap(_.execute(query)).map(_.errors).fork
          _     <- TestClock.adjust(1 minute)
          res   <- fiber.join
        } yield res)(equalTo(List(ExecutionError("""Query was interrupted after timeout of 1 m:

              {
                a
              }""".stripMargin))))
      },
      testM("Apollo Tracing") {
        case class Query(hero: Hero)
        case class Hero(name: URIO[Clock, String], friends: List[Hero] = Nil)

        object schema extends GenericSchema[Clock]
        import schema._

        def api(latch: Promise[Nothing, Unit]): GraphQL[Clock] =
          graphQL(
            RootResolver(
              Query(
                Hero(
                  latch.succeed(()) *> ZIO.sleep(1 second).as("R2-D2"),
                  List(
                    Hero(ZIO.sleep(2 second).as("Luke Skywalker")),
                    Hero(ZIO.sleep(3 second).as("Han Solo")),
                    Hero(ZIO.sleep(4 second).as("Leia Organa"))
                  )
                )
              )
            )
          ) @@ ApolloTracing.apolloTracing

        val query = gqldoc("""
              {
                hero {
                  name
                  friends {
                    name
                  }
                }
              }""")
        assertM(for {
          latch       <- Promise.make[Nothing, Unit]
          interpreter <- api(latch).interpreter
          fiber       <- interpreter.execute(query).map(_.extensions.map(_.toString)).fork
          _           <- latch.await
          _           <- TestClock.adjust(4 seconds)
          result      <- fiber.join
        } yield result)(
          isSome(
            equalTo(
              """{"tracing":{"version":1,"startTime":"1970-01-01T00:00:00.000Z","endTime":"1970-01-01T00:00:04.000Z","duration":4000000000,"parsing":{"startOffset":0,"duration":0},"validation":{"startOffset":0,"duration":0},"execution":{"resolvers":[{"path":["hero","name"],"parentType":"Hero","fieldName":"name","returnType":"String!","startOffset":0,"duration":1000000000},{"path":["hero","friends",0,"name"],"parentType":"Hero","fieldName":"name","returnType":"String!","startOffset":0,"duration":2000000000},{"path":["hero","friends",1,"name"],"parentType":"Hero","fieldName":"name","returnType":"String!","startOffset":0,"duration":3000000000},{"path":["hero"],"parentType":"Query","fieldName":"hero","returnType":"Hero!","startOffset":0,"duration":4000000000},{"path":["hero","friends"],"parentType":"Hero","fieldName":"friends","returnType":"[Hero!]!","startOffset":0,"duration":4000000000},{"path":["hero","friends",2,"name"],"parentType":"Hero","fieldName":"name","returnType":"String!","startOffset":0,"duration":4000000000}]}}}"""
            )
          )
        )
      },
      testM("Apollo Caching") {
        case class Query(@GQLDirective(CacheControl(10.seconds)) hero: Hero)

        @GQLDirective(CacheControl(2.seconds))
        case class Hero(name: URIO[Clock, String], friends: List[Hero] = Nil)

        object schema extends GenericSchema[Clock]
        import schema._

        def api: GraphQL[Clock] =
          graphQL(
            RootResolver(
              Query(
                Hero(
                  ZIO.succeed("R2-D2"),
                  List(
                    Hero(ZIO.succeed("Luke Skywalker")),
                    Hero(ZIO.succeed("Han Solo")),
                    Hero(ZIO.succeed("Leia Organa"))
                  )
                )
              )
            )
          ) @@ ApolloCaching.apolloCaching

        val query = gqldoc("""
              {
                hero {
                  name
                  friends {
                    name
                  }
                }
              }""")
        assertM(for {
          interpreter <- api.interpreter
          result      <- interpreter.execute(query).map(_.extensions.map(_.toString))
        } yield result)(
          isSome(
            equalTo(
              "{\"cacheControl\":{\"version\":1,\"hints\":[{\"path\":[\"hero\"],\"maxAge\":10,\"scope\":\"PRIVATE\"}]}}"
            )
          )
        )
      },
      suite("Apollo Persisted Queries")(
        testM("hash not found") {
          case class Test(test: String)
          val interpreter = (graphQL(RootResolver(Test("ok"))) @@ apolloPersistedQueries).interpreter
          assertM(
            interpreter
              .flatMap(
                _.executeRequest(
                  GraphQLRequest(extensions =
                    Some(Map("persistedQuery" -> ObjectValue(Map("sha256Hash" -> StringValue("my-hash")))))
                  )
                )
              )
              .map(_.asJson.noSpaces)
          )(equalTo("""{"data":null,"errors":[{"message":"PersistedQueryNotFound"}]}"""))
            .provideLayer(ApolloPersistedQueries.live)
        },
        testM("hash found") {
          case class Test(test: String)

          (for {
            interpreter <- (graphQL(RootResolver(Test("ok"))) @@ apolloPersistedQueries).interpreter
            extensions   = Some(Map("persistedQuery" -> ObjectValue(Map("sha256Hash" -> StringValue("my-hash")))))
            _           <- interpreter.executeRequest(GraphQLRequest(query = Some("{test}"), extensions = extensions))
            result      <- interpreter.executeRequest(GraphQLRequest(extensions = extensions))
          } yield assert(result.asJson.noSpaces)(equalTo("""{"data":{"test":"ok"}}""")))
            .provideLayer(ApolloPersistedQueries.live)
        }
      ),
      testM("custom query directive") {
        val customWrapper        = ExecutionWrapper[Any](f =>
          request => {
            if (request.field.directives.exists(_.name == "customQueryDirective")) {
              UIO {
                GraphQLResponse(Value.BooleanValue(true), Nil)
              }
            } else f(request)
          }
        )
        val customQueryDirective = __Directive(
          "customQueryDirective",
          None,
          Set(
            __DirectiveLocation.QUERY
          ),
          Nil
        )
        val interpreter          = (graphQL(
          resolver,
          List(
            customQueryDirective
          )
        ) @@ customWrapper).interpreter
        val query                = gqldoc("""
            query @customQueryDirective {
              characters {
                name
              }
            }""")
        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""true""")
        )
      }
    )
}
