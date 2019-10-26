package zquery

import zio.{ console, ZIO }
import zio.console.Console
import zio.test._
import zio.test.environment.TestConsole
import zio.test.Assertion._

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
          val a = getUserNameById(1).zip(getUserNameById(2)).mapError("identity")(identity)
          val b = getUserNameById(3).zip(getUserNameById(4)).mapError("identity")(identity)
          for {
            result <- ZQuery.collectAllPar(List(a, b)).run
            log    <- TestConsole.output
          } yield assert(log, hasSize(equalTo(2)))
        }
      )
    )

object ZQuerySpecUtil {

  val userIds: List[Int]          = (1 to 26).toList
  val userNames: Map[Int, String] = userIds.zip(('a' to 'z').map(_.toString)).toMap

  sealed trait UserRequest[+A] extends Request[A]

  case object GetAllIds                 extends UserRequest[List[Int]]
  final case class GetNameById(id: Int) extends UserRequest[String]

  object UserRequestDataSource extends DataSource[Console, Nothing, UserRequest[Any]] {
    val dataSource = new DataSource.Service[Console, Nothing, UserRequest[Any]] {
      val identifier = "UserRequestDataSource"
      def run(requests: Iterable[UserRequest[Any]]): ZIO[Console, Nothing, CompletedRequestMap] =
        console.putStrLn("Running query") *>
          ZIO.succeed {
            requests.foldLeft(CompletedRequestMap.empty) {
              case (completedRequests, GetAllIds)       => completedRequests.insert(GetAllIds)(userIds)
              case (completedRequests, GetNameById(id)) => completedRequests.insert(GetNameById(id))(userNames(id))
            }
          }
    }
  }

  val getAllUserIds: ZQuery[Console, Nothing, List[Int]] =
    ZQuery.fromRequestWith(GetAllIds)(UserRequestDataSource)

  def getUserNameById(id: Int): ZQuery[Console, Nothing, String] =
    ZQuery.fromRequestWith(GetNameById(id))(UserRequestDataSource)

  val getAllUserNames: ZQuery[Console, Nothing, List[String]] =
    for {
      userIds   <- getAllUserIds
      userNames <- ZQuery.foreachPar(userIds)(getUserNameById)
    } yield userNames
}
