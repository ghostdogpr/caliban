package example.interop.cats

import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban._
import caliban.interop.cats.ToEffect
import caliban.interop.cats.implicits._
import caliban.interop.fs2.implicits._
import caliban.schema.Schema.auto._
import cats.effect.std.{ Console, Dispatcher, Random }
import cats.effect.syntax.all._
import cats.effect.{ ExitCode, IO, IOApp, LiftIO }
import fs2.Stream
import zio.ZIO
import zio.interop.catz._
import zio.stream.ZStream
import zio.stream.interop.fs2z._

import scala.concurrent.duration._

object ExampleCatsInterop extends IOApp.Simple {

  implicit val zioRuntime: zio.Runtime[Any] = zio.Runtime.default

  val console = Console[IO]
  val random  = Random.javaUtilConcurrentThreadLocalRandom[IO]

  case class Number(value: Int)

  case class Queries(numbers: List[Number], randomNumber: IO[Number], moreRandomNumbers: Stream[IO, Number])

  case class Subscriptions(numbers: Stream[IO, Number])

  val numbers             = List(1, 2, 3, 4).map(Number)
  val randomNumber        = random.nextInt.map(Number)
  val moreRandomNumbers   = Stream.repeatEval(randomNumber).take(5)
  val subscriptionNumbers = Stream.emits(5 to 8).covary[IO].spaced(500.millis).map(Number)

  val queries       = Queries(numbers, randomNumber, moreRandomNumbers)
  val subscriptions = Subscriptions(subscriptionNumbers)

  val query =
    """|{
       |  numbers {
       |    value
       |  }
       |
       |  randomNumber {
       |    value
       |  }
       |
       |  moreRandomNumbers {
       |    value
       |  }
       |}
       |""".stripMargin

  val subscription =
    """|subscription {
       |  numbers {
       |    value
       |  }
       |}
       |""".stripMargin

  override def run: IO[Unit] =
    Dispatcher.parallel[IO].use { implicit dispatcher => // required for a derivation of the schema
      val api = graphQL(RootResolver(Some(queries), Option.empty[Unit], Some(subscriptions)))

      for {
        interpreter <- api.interpreterAsync[IO]

        _      <- console.println("execute a query:")
        _      <- interpreter.checkAsync[IO](query)
        result <- interpreter.executeAsync[IO](query)
        _      <- console.println(result.data)

        _      <- console.println("subscribe to a stream:")
        _      <- interpreter.checkAsync[IO](subscription)
        result <- interpreter.executeAsync[IO](subscription)

        _ <- result.data match {
               case ObjectValue(("numbers", StreamValue(zstream)) :: Nil) =>
                 zstream.toFs2Stream
                   .translate(ToEffect[IO, Any].toEffectK)
                   .foreach(console.println)
                   .compile
                   .drain

               case _ => console.println(s"Wrong result: ${result.data}")
             }
      } yield ()
    }
}
