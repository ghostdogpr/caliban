package zquery

import zio.console.Console
import zio.test.Assertion._
import zio.test.TestAspect.silent
import zio.test._
import zio.test.environment.{ TestConsole, TestEnvironment }
import zio.{ console, Promise, ZIO }

object ZQuerySpec extends ZIOBaseSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("ZQuerySpec")(
      testM("N + 1 selects problem") {
        for {
          result <- getAllUserNames.run
          log    <- TestConsole.output
        } yield assert(log)(hasSize(equalTo(2)))
      },
      testM("mapError does not prevent batching") {
        import zio.CanFail.canFail
        val a = getUserNameById(1).zip(getUserNameById(2)).mapError(identity)
        val b = getUserNameById(3).zip(getUserNameById(4)).mapError(identity)
        for {
          result <- ZQuery.collectAllPar(List(a, b)).run
          log    <- TestConsole.output
        } yield assert(log)(hasSize(equalTo(2)))
      },
      testM("failure to complete request is query failure") {
        for {
          result <- getUserNameById(27).run.run
        } yield assert(result)(dies(equalTo(QueryFailure(UserRequestDataSource, GetNameById(27)))))
      },
      test("query failure is correctly reported") {
        val failure = QueryFailure(UserRequestDataSource, GetNameById(27))
        assert(failure.getMessage)(
          equalTo("Data source UserRequestDataSource did not complete request GetNameById(27).")
        )
      },
      testM("timed does not prevent batching") {
        val a = getUserNameById(1).zip(getUserNameById(2)).timed
        val b = getUserNameById(3).zip(getUserNameById(4))
        for {
          _   <- ZQuery.collectAllPar(List(a, b)).run
          log <- TestConsole.output
        } yield assert(log)(hasSize(equalTo(2)))
      },
      testM("optional converts a query to one that returns its value optionally") {
        for {
          result <- getUserNameById(27).map(identity).optional.run
        } yield assert(result)(isNone)
      },
      testM("queries to multiple data sources can be executed in parallel") {
        for {
          promise <- Promise.make[Nothing, Unit]
          _       <- (neverQuery <&> succeedQuery(promise)).run.fork
          _       <- promise.await
        } yield assertCompletes
      },
      testM("arbitrary effects can be executed in parallel") {
        for {
          promise <- Promise.make[Nothing, Unit]
          _       <- (ZQuery.never <&> ZQuery.fromEffect(promise.succeed(()))).run.fork
          _       <- promise.await
        } yield assertCompletes
      },
      testM("zipPar does not prevent batching") {
        for {
          result <- ZQuery.collectAllPar(List.fill(100)(getAllUserNames)).run
          log    <- TestConsole.output
        } yield assert(log)(hasSize(equalTo(2)))
      } @@ TestAspect.nonFlaky
    ) @@ silent

  val userIds: List[Int]          = (1 to 26).toList
  val userNames: Map[Int, String] = userIds.zip(('a' to 'z').map(_.toString)).toMap

  sealed trait UserRequest[+A] extends Request[Nothing, A]

  case object GetAllIds                 extends UserRequest[List[Int]]
  final case class GetNameById(id: Int) extends UserRequest[String]

  val UserRequestDataSource: DataSource[Console, UserRequest[Any]] =
    DataSource[Console, UserRequest[Any]]("UserRequestDataSource") { requests =>
      ZIO.when(requests.toSet.size != requests.size)(ZIO.dieMessage("Duplicate requests)")) *>
        console.putStrLn("Running query") *>
        ZIO.succeed {
          requests.foldLeft(CompletedRequestMap.empty) {
            case (completedRequests, GetAllIds) => completedRequests.insert(GetAllIds)(Right(userIds))
            case (completedRequests, GetNameById(id)) =>
              userNames.get(id).fold(completedRequests)(name => completedRequests.insert(GetNameById(id))(Right(name)))
          }
        }
    }

  val getAllUserIds: ZQuery[Console, Nothing, List[Int]] =
    ZQuery.fromRequest(GetAllIds)(UserRequestDataSource)

  def getUserNameById(id: Int): ZQuery[Console, Nothing, String] =
    ZQuery.fromRequest(GetNameById(id))(UserRequestDataSource)

  val getAllUserNames: ZQuery[Console, Nothing, List[String]] =
    for {
      userIds   <- getAllUserIds
      userNames <- ZQuery.foreachPar(userIds)(getUserNameById)
    } yield userNames

  case object NeverRequest extends Request[Nothing, Nothing]

  val neverDataSource: DataSource[Any, NeverRequest.type] =
    DataSource.fromFunctionM("never")(_ => ZIO.never)

  val neverQuery: ZQuery[Any, Nothing, Nothing] =
    ZQuery.fromRequest(NeverRequest)(neverDataSource)

  final case class SucceedRequest(promise: Promise[Nothing, Unit]) extends Request[Nothing, Unit]

  val succeedDataSource: DataSource[Any, SucceedRequest] =
    DataSource.fromFunctionM("succeed") {
      case SucceedRequest(promise) => promise.succeed(()).unit
    }

  def succeedQuery(promise: Promise[Nothing, Unit]): ZQuery[Any, Nothing, Unit] =
    ZQuery.fromRequest(SucceedRequest(promise))(succeedDataSource)
}
