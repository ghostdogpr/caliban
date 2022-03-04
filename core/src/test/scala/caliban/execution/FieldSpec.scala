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
import caliban.parsing.adt.Selection

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

  def api(ref: Ref[Set[String]]) = {
    val api = graphQL(
      RootResolver(
        Queries(
          A(field => ref.set(field.targets.getOrElse(Set.empty)).as("id-a")),
          C("id-c")
        )
      )
    )

    api.interpreter
  }

  def spec: ZSpec[Environment, Failure] = suite("FieldSpec")(
    testM("gets populated with inline fragments") {
      val query = gqldoc("""{
              union { ...on Interface { id }  }
            }""")

      for {
        ref    <- Ref.make[Set[String]](Set.empty)
        i      <- api(ref)
        _      <- i.execute(query)
        actual <- ref.get
      } yield assertTrue(actual == Set("Interface"))
    },
    testM("doesn't get populated with mismatching type conditions") {
      val query = gqldoc("""{
              union { ...on B { id }  }
            }""")

      for {
        ref    <- Ref.make[Set[String]](Set.empty)
        i      <- api(ref)
        _      <- i.execute(query)
        actual <- ref.get
      } yield assertTrue(actual == Set.empty[String])
    },
    testM("gets populated with named fragment") {
      val query = gqldoc("""
        fragment Frag on A {
          id
        }
        {
          union { ...Frag }
        }""")

      for {
        ref    <- Ref.make[Set[String]](Set.empty)
        i      <- api(ref)
        _      <- i.execute(query)
        actual <- ref.get
      } yield assertTrue(actual == Set("A"))
    },
    testM("gets populated with unnamed fragment") {
      val query = gqldoc("""
        {
          union { ... { id } }
        }""")

      for {
        ref    <- Ref.make[Set[String]](Set.empty)
        i      <- api(ref)
        _      <- i.execute(query)
        actual <- ref.get
      } yield assertTrue(actual == Set.empty[String])
    }
  )
}
