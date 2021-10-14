package caliban.validation

import caliban.CalibanError
import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.RootResolver
import caliban.schema.Annotations.GQLDefault
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

import java.util.UUID

object InputObjectSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("InputObjectSpect")(
      testM("fails is a non-null field on an input object is null") {
        val query =
          """query {
            |  query(input: {string: null})
            |}""".stripMargin

        case class TestInputObject(string: String)
        case class TestInput(input: TestInputObject)
        case class Query(query: TestInput => String)
        val gql = graphQL(RootResolver(Query(_.input.string)))

        for {
          int <- gql.interpreter
          res <- int.execute(query)
        } yield assert(res.errors.headOption)(
          isSome((isSubtype[CalibanError.ValidationError](anything)))
        )
      }
    )
}
