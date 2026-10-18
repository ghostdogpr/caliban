package caliban.gateway

import zio._
import zio.test._

/**
 * Specs run on the test clock, which stops the gateway's own timeouts: an HTTP exchange that the loopback stack stalls
 * on a loaded machine would wait forever. A test still running after a minute of real time runs once more.
 */
abstract class GatewaySpecDefault extends ZIOSpecDefault {
  override def aspects: Chunk[TestAspectAtLeastR[TestEnvironment]] =
    super.aspects :+ (TestAspect.timeout(1.minute) >>> new TestAspect.PerTest.Poly {
      def perTest[R, E](test: ZIO[R, TestFailure[E], TestSuccess])(implicit
        trace: Trace
      ): ZIO[R, TestFailure[E], TestSuccess] =
        test.catchSome {
          case TestFailure.Runtime(cause, _) if cause.dieOption.exists(_.isInstanceOf[TestTimeoutException]) => test
        }
    })
}
