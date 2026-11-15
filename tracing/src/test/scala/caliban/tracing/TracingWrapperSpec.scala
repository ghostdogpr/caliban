package caliban.tracing

import caliban._
import caliban.Macros.gqldoc
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.auto._
import zio._
import zio.query._
import zio.telemetry.opentelemetry.tracing.Tracing
import zio.test._

import scala.jdk.CollectionConverters._

object TracingWrapperSpec extends ZIOSpecDefault {
  val randomSleep: ZIO[Any, Nothing, Unit] = Random.nextIntBetween(0, 500).flatMap(t => ZIO.sleep(t.millis))

  final case class GetName(name: String) extends Request[Throwable, String]

  val NamesDatasource = new DataSource.Batched[Any, GetName] {
    override val identifier: String                                                                               = "NamesDatasource"
    override def run(requests: Chunk[GetName])(implicit trace: zio.Trace): ZIO[Any, Nothing, CompletedRequestMap] =
      ZIO.succeed {
        CompletedRequestMap.fromIterableWith(requests)(identity, req => Exit.succeed(req.name))
      }
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
    test("traces the batched steps, continuing the incoming trace unless a span is active") {
      val traceId  = "4bf92f3577b34da6a3ce929b0e0e4736"
      val parentId = "00f067aa0ba902b7"
      val headers  = List(
        "TraceParent" -> s"00-$traceId-$parentId-01",
        "tracestate"  -> "vendor1=first",
        "TraceState"  -> "vendor2=second"
      )
      val query    = gqldoc("""{
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
        fiber       <- IncomingRequestHeaders.locally(headers)(interpreter.execute(query)).fork
        _           <- TestClock.adjust(30.seconds)
        res         <- fiber.join
        spans       <- TracingMock.getFinishedSpans
        tracing     <- ZIO.service[Tracing]
        _           <-
          tracing.span("outer")(IncomingRequestHeaders.locally(headers)(interpreter.execute(gqldoc("{ __typename }"))))
        nested      <- TracingMock.getFinishedSpans.map(_.drop(spans.length))
      } yield assertTrue(res.errors.isEmpty) &&
        assertTrue(
          spans.map(_.getName()) == List("age", "age", "age", "age", "name", "friends", "person", "query"),
          spans.forall(_.getTraceId == traceId),
          spans.last.getParentSpanId == parentId,
          spans.last.getParentSpanContext.getTraceState.asMap == Map(
            "vendor1" -> "first",
            "vendor2" -> "second"
          ).asJava,
          nested.map(_.getName()) == List("query", "outer"),
          nested.head.getParentSpanId == nested.last.getSpanId
        )
    }.provide(
      TracingMock.layer
    )
  )
}
