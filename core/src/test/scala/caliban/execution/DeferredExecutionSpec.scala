package caliban.execution

import zio.test.DefaultRunnableSpec
import zio.test.ZSpec

import caliban.TestUtils._

object DeferredExecutionSpec extends DefaultRunnableSpec {

    override def spec = suite("deferred execution")(
        testM("can execute a deferred query") {
            val query       = gqldoc("""
                query test {
                    character(name: "Amos Burton") {
                        ...CharacterFragement @defer(label:"test")
                    }
                }

                fragment CharacterFragment {
                    name
                    nicknames @skip(if: true)
                }
            """)

            for {
              interpreter <- graphQL(resolver).interpreter
            } yield ()
        }
    )
  
}
