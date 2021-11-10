package caliban.validation

import caliban.CalibanError
import caliban.GraphQL._
import caliban.GraphQLRequest
import caliban.{ InputValue, Value }
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
      },
      testM("fails if a non-null field on an input object is null from variables") {
        val query =
          """query QueryName($input: TestInputObjectInput!) {
            |  query(input: $input)
            |}""".stripMargin

        case class TestInputObject(string: String)
        case class TestInput(input: TestInputObject)
        case class Query(query: TestInput => String)
        val gql = graphQL(RootResolver(Query(_.input.string)))

        for {
          int <- gql.interpreter
          res <- int.executeRequest(
                   GraphQLRequest(
                     query = Some(query),
                     variables = Some(
                       Map(
                         "input" -> InputValue.ObjectValue(
                           Map("string" -> Value.NullValue)
                         )
                       )
                     )
                   )
                 )
        } yield assert(res.errors.headOption)(
          isSome((isSubtype[CalibanError.ValidationError](anything)))
        )
      },
      testM("not fails if a null passed to optional list from variables") {
        val query =
          """query QueryName($input: TestInputObjectInput!) {
            |  query(input: $input)
            |}""".stripMargin

        case class TestInputObject(list: Option[List[Long]])
        case class TestInput(input: TestInputObject)
        case class TestOutput(value: String)
        case class Query(query: TestInput => String)
        val gql = graphQL(RootResolver(Query(_.input.list.fold("null")(_.mkString(",")))))

        for {
          int <- gql.interpreter
          res <- int.executeRequest(
                   GraphQLRequest(
                     query = Some(query),
                     variables = Some(
                       Map(
                         "input" -> InputValue.ObjectValue(
                           Map("list" -> Value.NullValue)
                         )
                       )
                     )
                   )
                 )
        } yield assert(res.errors)(isEmpty)
      }
    )
}
