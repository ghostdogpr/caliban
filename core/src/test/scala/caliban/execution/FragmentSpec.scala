package caliban.execution

import caliban.CalibanError
import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.RootResolver
import caliban.schema.Annotations.GQLDefault
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment
import caliban.TestUtils._

object FragmentSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("FragmentSpec")(
      testM("fragments") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
                   {
                     amos: character(name: "Amos Burton") {
                       ...info
                     }
                   }

                   fragment info on Character {
                     name
                   }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"amos":{"name":"Amos Burton"}}""")
        )
      },
      testM("fragment on union") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
                   {
                     amos: character(name: "Amos Burton") {
                       role {
                         ...roleF
                       }
                     }
                   }

                   fragment roleF on Role {
                     ... on Mechanic {
                       shipName
                     }
                   }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"amos":{"role":{"shipName":"Rocinante"}}}""")
        )
      },
      testM("inline fragment") {
        val interpreter = graphQL(resolver).interpreter
        val query       = gqldoc("""
                   {
                     amos: character(name: "Amos Burton") {
                       name
                       role {
                         ... on Mechanic {
                           shipName
                         }
                       }
                     }
                   }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(
          equalTo("""{"amos":{"name":"Amos Burton","role":{"shipName":"Rocinante"}}}""")
        )
      },
      testM("inline fragment selection with equal field types") {
        sealed trait Union
        case class A(name: String) extends Union
        case class B(name: String) extends Union

        val query =
          """query{
            |  test {
            |    ...on A { name }
            |    ...on B { name }
            |   }
            |}""".stripMargin

        case class Query(test: Union)
        val gql = graphQL(RootResolver(Query(A("Name"))))
        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assert(res.errors)(
          isEmpty
        )
      },
      testM("inline fragment selection with different field types") {
        sealed trait Union
        case class A(name: String)         extends Union
        case class B(name: Option[String]) extends Union

        val query =
          """query{
            |  test {
            |    ...on A { name }
            |    ...on B { name }
            |   }
            |}""".stripMargin

        case class Query(test: Union)
        val gql = graphQL(RootResolver(Query(A("Name"))))
        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assert(res.errors.headOption)(isSome(anything))
      },
      testM("inline fragment selection with same arguments") {
        sealed trait Union
        case class A(name: String => String) extends Union
        case class B(name: String => String) extends Union

        val query =
          """query{
            |  test {
            |    ...on A { name(value: "hi") }
            |    ...on B { name(value: "hi") }
            |   }
            |}""".stripMargin

        case class Query(test: Union)
        val gql = graphQL(RootResolver(Query(A(_ => "Name"))))
        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assert(res.errors)(isEmpty)
      },
      testM("inline fragment selection with different arguments") {
        sealed trait Union
        case class A(name: Int => String)    extends Union
        case class B(name: String => String) extends Union

        val query =
          """query{
            |  test {
            |    ...on A { name(value: 1) }
            |    ...on B { name(value: "hi") }
            |   }
            |}""".stripMargin

        case class Query(test: Union)
        val gql = graphQL(RootResolver(Query(A(_ => "Name"))))
        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assert(res.errors.headOption)(
          isSome(anything)
        )
      }
    )
}
