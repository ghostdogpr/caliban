package zquery

import zio.console.Console
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestConsole
import zio.{ console, ZIO }
import zquery.ZQuerySpecUtil._

object ZQuerySpec
    extends ZIOBaseSpec(
      suite("ZQuerySpec")(
        testM("N + 1 selects problem") {
          for {
            result <- getAllUserNames.run
            log    <- TestConsole.output
          } yield assert(log, hasSize(equalTo(2)))
        },
        testM("mapError does not prevent batching") {
          import zio.CanFail.canFail
          val a = getUserNameById(1).zip(getUserNameById(2)).mapError(identity)
          val b = getUserNameById(3).zip(getUserNameById(4)).mapError(identity)
          for {
            result <- ZQuery.collectAllPar(List(a, b)).run
            log    <- TestConsole.output
          } yield assert(log, hasSize(equalTo(2)))
        },
        testM("failure to complete request is query failure") {
          for {
            result <- getUserNameById(27).run.run
          } yield assert(result, dies(equalTo(QueryFailure(UserRequestDataSource, GetNameById(27)))))
        },
        test("query failure is correctly reported") {
          val failure = QueryFailure(UserRequestDataSource, GetNameById(27))
          assert(
            failure.getMessage,
            equalTo("Data source UserRequestDataSource did not complete request GetNameById(27).")
          )
        }
      )
    )

object ZQuerySpecUtil {

  val userIds: List[Int]          = (1 to 26).toList
  val userNames: Map[Int, String] = userIds.zip(('a' to 'z').map(_.toString)).toMap

  sealed trait UserRequest[+A] extends Request[Nothing, A]

  case object GetAllIds                 extends UserRequest[List[Int]]
  final case class GetNameById(id: Int) extends UserRequest[String]

  val UserRequestDataSource =
    DataSource[Console, UserRequest[Any]]("UserRequestDataSource") { requests =>
      console.putStrLn("Running query") *> ZIO.succeed {
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
}
