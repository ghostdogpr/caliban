package caliban.schema

import caliban.schema.Annotations.GQLField
import caliban.{ graphQL, RootResolver }
import zio.test._
import zio.{ Task, ZIO }

// Executing methods-as-fields requires java.lang.reflect.Method.invoke, which is JVM-only.
// On Scala Native this scenario throws UnsupportedOperationException at resolve time.
object Scala3DerivesMethodExecutionSpec extends ZIOSpecDefault {
  override def spec = suite("Scala3DerivesSpec - method execution (JVM-only)")(
    test("execution of methods as fields") {
      final case class Foo(value: String) derives Schema.SemiAuto {
        @GQLField def value2: Task[String] = ZIO.succeed(value + 2)
      }
      final case class Bar(foo: Foo) derives Schema.SemiAuto
      val gql = graphQL(RootResolver(Bar(Foo("foo"))))

      gql.interpreter.flatMap { i =>
        i.execute("{foo {value value2}}").map { v =>
          val s = v.data.toString
          assertTrue(s == """{"foo":{"value":"foo","value2":"foo2"}}""")
        }
      }
    }
  )
}
