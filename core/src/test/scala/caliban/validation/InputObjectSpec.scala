package caliban.validation

import caliban.{ CalibanError, GraphQLRequest, InputValue, RootResolver, TestUtils, Value }
import caliban.GraphQL._
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
          isSome(isSubtype[CalibanError.ValidationError](anything))
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
          isSome(isSubtype[CalibanError.ValidationError](anything))
        )
      },
      testM("allow null passed to optional enum") {
        val query =
          """query QueryName($input: TestInputObjectInput!) {
            |  query(input: $input)
            |}""".stripMargin

        case class TestInputObject(enumField: Option[TestUtils.Origin])
        case class TestInput(input: TestInputObject)
        case class TestOutput(value: String)
        case class Query(query: TestInput => String)
        val gql = graphQL(RootResolver(Query(_.input.enumField.fold("null")(_.toString))))

        for {
          int <- gql.interpreter
          res <- int.executeRequest(
                   GraphQLRequest(
                     query = Some(query),
                     variables = Some(
                       Map(
                         "input" -> InputValue.ObjectValue(
                           Map("enumField" -> Value.NullValue)
                         )
                       )
                     )
                   )
                 )
        } yield assert(res.errors)(isEmpty)
      },
      testM("allow null passed to optional object") {
        val query =
          """query QueryName($input: TestInputObjectInput!) {
            |  query(input: $input)
            |}""".stripMargin

        case class TestInputObject(obj: Option[TestUtils.Painter])
        case class TestInput(input: TestInputObject)
        case class TestOutput(value: String)
        case class Query(query: TestInput => String)
        val gql = graphQL(RootResolver(Query(_.input.obj.fold("null")(_.toString))))

        for {
          int <- gql.interpreter
          res <- int.executeRequest(
                   GraphQLRequest(
                     query = Some(query),
                     variables = Some(
                       Map(
                         "input" -> InputValue.ObjectValue(
                           Map("obj" -> Value.NullValue)
                         )
                       )
                     )
                   )
                 )
        } yield assert(res.errors)(isEmpty)
      },
      testM("allow null passed to optional list") {
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
      },
      testM("allow Int passed to Float field") {
        val query =
          """query {
            |  query(input: { float: 42 })
            |}""".stripMargin

        case class TestInputObject(float: Float)
        case class TestInput(input: TestInputObject)
        case class TestOutput(value: String)
        case class Query(query: TestInput => String)
        val gql = graphQL(RootResolver(Query(_.input.float.toString)))

        for {
          int <- gql.interpreter
          res <- int.executeRequest(GraphQLRequest(query = Some(query)))
        } yield assert(res.errors)(isEmpty)
      },
      testM("allow Int passed to Float field in a variable") {
        val query =
          """query QueryName($input: TestInputObjectInput!) {
            |  query(input: $input)
            |}""".stripMargin

        case class TestInputObject(float: Float)
        case class TestInput(input: TestInputObject)
        case class TestOutput(value: String)
        case class Query(query: TestInput => String)
        val gql = graphQL(RootResolver(Query(_.input.float.toString)))

        for {
          int <- gql.interpreter
          res <- int.executeRequest(
                   GraphQLRequest(
                     query = Some(query),
                     variables = Some(
                       Map(
                         "input" -> InputValue.ObjectValue(
                           Map("float" -> Value.IntValue(42))
                         )
                       )
                     )
                   )
                 )
        } yield assert(res.errors)(isEmpty)
      }
    )
}
