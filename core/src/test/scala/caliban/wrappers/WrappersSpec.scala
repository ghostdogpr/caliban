package caliban.wrappers

import scala.language.postfixOps
import caliban.CalibanError.ExecutionError
import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.{ CalibanError, GraphQLInterpreter, RootResolver }
import caliban.schema.GenericSchema
import zio.clock.Clock
import zio.duration._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestClock
import zio.{ clock, Promise, UIO, URIO, ZIO }

object WrappersSpec
    extends DefaultRunnableSpec(
      suite("WrappersSpec")(
        testM("Timeout") {
          case class Test(a: URIO[Clock, Int])

          object schema extends GenericSchema[Clock]
          import schema._

          val interpreter =
            Wrappers
              .timeout(1 minute)(
                graphQL(RootResolver(Test(clock.sleep(2 minutes).as(0))))
              )
              .interpreter
          val query = gqldoc("""
              {
                a
              }""")
          assertM(
            TestClock.adjust(1 minute) *> interpreter.execute(query).map(_.errors),
            equalTo(List(ExecutionError("""Query was interrupted after timeout of 1 m:

              {
                a
              }""".stripMargin)))
          )
        },
        testM("Apollo Tracing") {
          case class Query(hero: Hero)
          case class Hero(name: URIO[Clock, String], friends: List[Hero] = Nil)

          object schema extends GenericSchema[Clock]
          import schema._

          def interpreter(latch: Promise[Nothing, Unit]): UIO[GraphQLInterpreter[Clock, CalibanError]] =
            ApolloTracing
              .apolloTracing(
                graphQL(
                  RootResolver(
                    Query(
                      Hero(
                        latch.succeed(()) *> ZIO.sleep(1 second).as("R2-D2"),
                        List(
                          Hero(ZIO.succeed("Luke Skywalker")),
                          Hero(ZIO.succeed("Han Solo")),
                          Hero(ZIO.succeed("Leia Organa"))
                        )
                      )
                    )
                  )
                )
              )
              .map(_.interpreter)

          val query = gqldoc("""
              {
                hero {
                  name
                  friends {
                    name
                  }
                }
              }""")
          assertM(
            for {
              latch       <- Promise.make[Nothing, Unit]
              interpreter <- interpreter(latch)
              fiber       <- interpreter.execute(query).map(_.extensions.map(_.toString)).fork
              _           <- latch.await
              _           <- TestClock.adjust(1 second)
              result      <- fiber.join
            } yield result,
            isSome(
              equalTo(
                """{"tracing":{"version":1,"startTime":"1970-01-01T00:00:00.000Z","endTime":"1970-01-01T00:00:01.000Z","duration":1000000000,"parsing":{"startOffset":0,"duration":0},"validation":{"startOffset":0,"duration":0},"execution":{"resolvers":[{"path":["hero"],"parentType":"Query","fieldName":"hero","returnType":"Hero!","startOffset":0,"duration":1000000000},{"path":["hero","name"],"parentType":"Hero","fieldName":"name","returnType":"String!","startOffset":0,"duration":1000000000},{"path":["hero","friends"],"parentType":"Hero","fieldName":"friends","returnType":"[Hero!]!","startOffset":1000000000,"duration":0},{"path":["hero","friends",2,"name"],"parentType":"Hero","fieldName":"name","returnType":"String!","startOffset":1000000000,"duration":0},{"path":["hero","friends",1,"name"],"parentType":"Hero","fieldName":"name","returnType":"String!","startOffset":1000000000,"duration":0},{"path":["hero","friends",0,"name"],"parentType":"Hero","fieldName":"name","returnType":"String!","startOffset":1000000000,"duration":0}]}}}"""
              )
            )
          )
        }
      )
    )
