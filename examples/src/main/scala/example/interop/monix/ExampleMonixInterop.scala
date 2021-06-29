package example.interop.monix

import caliban.GraphQL.graphQL
import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.RootResolver

import cats.effect.ExitCode
import monix.eval.{ Task, TaskApp }
import monix.reactive.Observable
import zio.{ Runtime, ZEnv }
import zio.interop.reactivestreams._

object ExampleMonixInterop extends TaskApp {

  import caliban.interop.monix.implicits._

  implicit val runtime: Runtime[ZEnv] = Runtime.default

  case class Number(value: Int)
  case class Queries(numbers: List[Number], randomNumber: Task[Number])
  case class Subscriptions(numbers: Observable[Int])

  val numbers      = List(1, 2, 3, 4).map(Number)
  val randomNumber = Task.eval(scala.util.Random.nextInt()).map(Number)
  val queries      = Queries(numbers, randomNumber)

  val subscriptions = Subscriptions(Observable.fromIterable(List(1, 2, 3)))
  val api           = graphQL(RootResolver(Some(queries), Option.empty[Unit], Some(subscriptions)))

  val query = """
  {
    numbers {
      value
    }

    randomNumber {
      value
    }
  }"""

  val subscription = """
  subscription {
    numbers
  }"""

  override def run(args: List[String]): Task[ExitCode] =
    for {
      interpreter <- api.interpreterAsync
      _           <- interpreter.checkAsync(query)
      result      <- interpreter.executeAsync(query)
      _           <- Task.eval(println(result.data))

      _      <- interpreter.checkAsync(subscription)
      result <- interpreter.executeAsync(subscription)
      _ <- result.data match {
            case ObjectValue(("numbers", StreamValue(stream)) :: Nil) =>
              // get back an observable
              val obs = Observable.fromReactivePublisher(runtime.unsafeRun(stream.toPublisher))
              obs.foreachL(println)
            case _ => Task.eval(println(s"Wrong result: ${result.data}"))
          }
    } yield ExitCode.Success
}
