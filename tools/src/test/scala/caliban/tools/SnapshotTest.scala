package caliban.tools

import caliban.tools.SnapshotTest.GitLock
import zio.internal.stacktracer.SourceLocation
import zio.test._
import zio.{ Task, Trace }

import java.nio.file.{ Files, Path }
import scala.util.{ Failure, Success, Try }

trait SnapshotTest extends ZIOSpecDefault {
  def testName: String

  def snapshotTest(
    label0: String
  )(str: Task[String])(implicit sourceLocation: SourceLocation, trace: Trace): Spec[Any, Throwable] = {
    val label = label0.replace('/', '_').replace("'", "")
    zio.test.test[Task[TestResult]](label) {
      str.map { str =>
        val isCi = SnapshotTest.isCi
        val path = SnapshotTest.projectRoot.resolve(s"tools/src/test/resources/snapshots/$testName/${label + ".scala"}")

        def write(): TestResult = {
          Files.createDirectories(path.getParent)
          Files.writeString(path, str)
          import scala.sys.process._
          // at least don't take the git lock multiple times from same process. this can still fail if concurrent processes try to take it.
          GitLock.synchronized {
            // allow failing external command, but complain to stderr
            try s"git add '$path'".!
            catch {
              case th: Throwable =>
                System.err.println(s"Could not add snapshot file '$path' to git: ${th.getMessage}")
            }
          }
          assert(())(Assertion.anything)
        }

        Try(Files.readString(path)) match {
          case Success(existing) if isCi =>
            assertTrue(str == existing).label(
              s"generated result for test '$label' did not match snapshot contents in file '$path. Rerun with environment `CI` not set to 'true' to update and then check in the file"
            )
          case Success(_)                =>
            write()
          case Failure(_) if isCi        =>
            assertTrue(false).label(
              s"Could not read snapshot file '$path'. Rerun with environment `CI` not set to 'true' to create and then check in the file"
            )
          case Failure(_)                =>
            write()
        }
      }
    } @@ TestAspect.blocking
  }

}

object SnapshotTest {
  val `.git`: Path = Path.of(".git")
  val cwd: Path    = Path.of(sys.props("user.dir"))

  val projectRoot: Path = {
    def lookUpwards(p: Path): Option[Path] =
      if (Files.list(p).anyMatch(p => p.getFileName == `.git`)) Some(p)
      else Option(p.getParent).flatMap(lookUpwards)

    lookUpwards(cwd).getOrElse {
      sys.error(s"cannot find root (uses ${`.git`} as a hint)")
    }
  }

  val isCi: Boolean =
    sys.env.contains("BUILD_NUMBER") || sys.env.contains("CI") // from sbt

  private object GitLock
}
