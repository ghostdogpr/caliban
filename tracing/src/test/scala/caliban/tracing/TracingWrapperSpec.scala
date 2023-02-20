package caliban.tracing

import caliban.GraphQL.graphQL
import caliban.Macros.gqldoc
import caliban.RootResolver
import zio._
import zio.query._
import zio.test._

import Assertion._

object TracingWrapperSpec extends ZIOSpecDefault {
  val randomSleep: ZIO[Any, Nothing, Unit] = Random.nextIntBetween(0, 500).flatMap(t => ZIO.sleep(t.millis))

  final case class GetName(name: String) extends Request[Throwable, String]

  val NamesDatasource = new DataSource.Batched[Any, GetName] {
    override val identifier: String                                                                               = "NamesDatasource"
    override def run(requests: Chunk[GetName])(implicit trace: zio.Trace): ZIO[Any, Nothing, CompletedRequestMap] =
      ZIO.succeed(requests.foldLeft(CompletedRequestMap.empty) { case (map, req) =>
        map.insert(req)(Right(req.name))
      })
  }

  case class PersonArgs(name: String)
  case class Person(
    name: ZQuery[Any, Throwable, String],
    age: ZIO[Any, Nothing, Int],
    friends: ZIO[Any, Nothing, List[Friend]]
  )
  object Person {
    def apply(name: String): Person = new Person(
      name = ZQuery.fromZIO(randomSleep) *> ZQuery.fromRequest(GetName(name))(NamesDatasource),
      age = (Random.nextIntBetween(0, 100) zip Random.nextIntBetween(0, 500)).flatMap { case (i, d) =>
        ZIO.succeed(i).delay(d.milliseconds)
      },
      friends = randomSleep.as(List(Friend("Joe"), Friend("Bob"), Friend("Alice")))
    )
  }

  case class Friend(
    name: ZQuery[Any, Throwable, String],
    age: ZIO[Any, Nothing, Int]
  )
  object Friend {
    def apply(name: String): Friend = new Friend(
      name = ZQuery.fromZIO(randomSleep) *> ZQuery.fromRequest(GetName(name))(NamesDatasource),
      age = (Random.nextIntBetween(0, 100) zip Random.nextIntBetween(0, 500)).flatMap { case (i, d) =>
        ZIO.succeed(i).delay(d.milliseconds)
      }
    )
  }

  case class Queries(
    person: PersonArgs => ZIO[Any, Nothing, Person] = args => randomSleep *> ZIO.succeed(Person(args.name))
  )

  val api = graphQL(
    RootResolver(
      Queries()
    )
  ) @@ TracingWrapper.traced

  def spec = suite("TracingWrapperSpec")(
    test("traces the batched steps") {
      val query = gqldoc("""{
            person(name: "Carol") {
                name
                age
                friends {
                    name
                    age
                }
            }
        }""")
      for {
        interpreter <- api.interpreter
        fiber       <- interpreter.execute(query).fork
        _           <- TestClock.adjust(30.seconds)
        res         <- fiber.join
        spans       <- TracingMock.getFinishedSpans.map(_.map(_.getName()))
      } yield assertTrue(res.errors.isEmpty) &&
        // only two names since we're only measuring the batched calls
        assert(spans)(hasSameElements(List("name", "name", "person", "friends", "person", "query")))
    }.provide(
      TracingMock.layer
    )
  )
}
