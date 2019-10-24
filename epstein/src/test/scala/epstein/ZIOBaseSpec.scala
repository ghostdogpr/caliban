package epstein

import zio.duration._
import zio.test._
import zio.test.environment._

abstract class ZIOBaseSpec(spec: => ZSpec[TestEnvironment, Any, String, Any])
    extends DefaultRunnableSpec(spec, List(TestAspect.timeout(60.seconds)))
