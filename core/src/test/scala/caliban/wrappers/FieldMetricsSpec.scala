package caliban.wrappers

import caliban._
import caliban.Macros.gqldoc
import caliban.schema.Annotations.GQLDefault
import caliban.schema.ArgBuilder.auto._
import caliban.schema.Schema.auto._
import zio._
import zio.metrics.Metric
import zio.metrics.{ MetricLabel, MetricState }
import zio.test._

object FieldMetricsSpec extends ZIOSpecDefault {
  val randomSleep: ZIO[Any, Nothing, Unit] = Random.nextIntBetween(0, 500).flatMap(t => ZIO.sleep(t.millis))
  val buckets                              = FieldMetrics.defaultBuckets

  case class PersonArgs(name: String)
  case class NameArgs(@GQLDefault("500") delay: Int)
  case class Person(
    name: NameArgs => ZIO[Any, Nothing, String],
    age: ZIO[Any, Nothing, Int],
    friends: ZIO[Any, Nothing, List[Friend]]
  )
  object Person {
    def apply(name: String): Person = new Person(
      name = (args: NameArgs) => ZIO.sleep(args.delay.milliseconds).as(name),
      age = (Random.nextIntBetween(0, 100) zip Random.nextIntBetween(0, 500)).flatMap { case (i, d) =>
        ZIO.succeed(i).delay(d.milliseconds)
      },
      friends = randomSleep.as(List(Friend("Joe"), Friend("Bob"), Friend("Alice"), Friend("Unfriendly")))
    )
  }

  case class Friend(
    name: NameArgs => ZIO[Any, CalibanError.ExecutionError, String],
    age: ZIO[Any, Nothing, Int]
  )

  object Friend {
    def apply(name: String): Friend = new Friend(
      name = (args: NameArgs) =>
        if (name != "Unfriendly") ZIO.sleep(args.delay.milliseconds).as(name)
        else ZIO.fail(CalibanError.ExecutionError("Not a friend")),
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
  )

  def getCountForDuration(value: MetricState.Histogram, duration: Double) =
    value.buckets.find(_._1 == duration).map(_._2.toInt).get

  def spec: Spec[TestEnvironment with Scope, Any] = suite("FieldMetricsSpec")(
    test("measures the duration of a field correctly and increases the total metric with correct status") {
      val metric      =
        Metric.histogram("graphql_fields_duration_seconds", buckets).tagged("test", "success")
      val metricTotal =
        Metric.counter("graphql_fields_total").tagged("test", "success")
      val query       = gqldoc("""{
            person(name: "Carol") {
                name(delay: 100)
                age
                friends {
                    name
                    age
                }
            }
        }""")
      for {
        interpreter <- (api @@ Wrappers.metrics(extraLabels = Set(MetricLabel("test", "success")))).interpreter
        _           <- TestClock.adjust(30.seconds)
        fiber       <- interpreter.execute(query).fork
        _           <- TestClock.adjust(30.seconds)
        res         <- fiber.join
        root        <- metric.tagged("field", "Queries.person").value
        name        <- metric.tagged("field", "Person.name").value
        friendsName <- metric.tagged("field", "Friend.name").value
        totalRoot   <- metricTotal.tagged("field", "Queries.person").tagged("status", "ok").value
        total       <- metricTotal.tagged("field", "Friend.name").tagged("status", "ok").value
        totalError  <- metricTotal.tagged("field", "Friend.name").tagged("status", "error").value
      } yield assertTrue(
        res.errors.size == 1,
        getCountForDuration(root, 0.025) == 0,
        getCountForDuration(root, 0.05) == 1,
        getCountForDuration(root, 0.5) == 1,
        getCountForDuration(name, 0.075) == 0,
        getCountForDuration(name, 0.1) == 1,
        getCountForDuration(name, 0.5) == 1,
        getCountForDuration(friendsName, 0.25) == 0,
        getCountForDuration(friendsName, 0.5) == 3,
        totalRoot.count == 1.0,
        total.count == 3.0,
        totalError.count == 1.0
      )
    }
  )
}
