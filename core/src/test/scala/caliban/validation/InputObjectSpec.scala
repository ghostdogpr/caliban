package caliban.validation

import caliban.CalibanError
import caliban.GraphQL._
import caliban.RootResolver
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object InputObjectSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("InputObjectSpec")(
      testM("fails if a non-null field on an input object is null") {
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
