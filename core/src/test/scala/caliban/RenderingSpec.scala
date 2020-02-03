package caliban

import zio.test._
import Assertion._
import TestUtils._
import caliban.GraphQL._

object RenderingSpec
    extends DefaultRunnableSpec(
      suite("rendering")(
        test("it should render directives") {

          assert(graphQL(resolver).render, equalTo("bob"))

        }
      )
    )
