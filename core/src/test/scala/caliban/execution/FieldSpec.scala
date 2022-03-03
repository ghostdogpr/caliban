package caliban.execution

import caliban.GraphQL.graphQL
import caliban.Macros.gqldoc
import caliban.RootResolver
import caliban.schema.Annotations.GQLInterface
import caliban.schema._
import zio._
import zio.test.DefaultRunnableSpec
import zio.test.ZSpec
import zio.test._

import Assertion._

object FieldSpec extends DefaultRunnableSpec {

  sealed trait Union
  @GQLInterface
  sealed trait Interface

  case class A(id: Field => UIO[String]) extends Union with Interface

  case class B(id: String) extends Union with Interface

  case class C(id: String) extends Interface

  case class Queries(
    union: Union,
    interface: Interface
  )

  def spec: ZSpec[Environment, Failure] = suite("FieldSpec")(
    testM("fails") {
      val query = gqldoc("""{
          union { ...on Interface { id } }
        }""")

      for {
        ref <- Ref.make[Set[String]](Set.empty)
        api  = graphQL(
                 RootResolver(
                   Queries(
                     A(field => ref.set(field.condition.getOrElse(Set.empty)).as("id-a")),
                     C("id-c")
                   )
                 )
               )

        i      <- api.interpreter
        _      <- i.execute(query)
        actual <- ref.get
      } yield assertTrue(actual == Set("Interface"))
    }
  )
}
