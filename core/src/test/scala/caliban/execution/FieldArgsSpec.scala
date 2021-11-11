package caliban.execution

import caliban.GraphQL._
import caliban.{ GraphQLRequest, InputValue, RootResolver, Value }
import caliban.Value.EnumValue
import zio._
import zio.test._
import zio.test.environment.TestEnvironment
import zio.test.Assertion._

object FieldArgsSpec extends DefaultRunnableSpec {
  sealed trait COLOR
  object COLOR {
    case object GREEN extends COLOR
    case object BLUE  extends COLOR
  }

  override def spec: ZSpec[TestEnvironment, Any] = suite("FieldArgsSpec")(
    testM("it forward args of correct type") {
      case class QueryInput(color: COLOR, string: String)
      case class Query(query: Field => QueryInput => UIO[String])
      val query =
        """query{
          |  query(string: "test", color: BLUE)
          |}""".stripMargin

      for {
        ref         <- Ref.make[Option[Field]](None)
        api          = graphQL(
                         RootResolver(
                           Query(
                             query = info => i => ref.set(Option(info)).as(i.string)
                           )
                         )
                       )
        interpreter <- api.interpreter
        _           <- interpreter.execute(query)

        res <- ref.get
      } yield assertTrue(
        res.get.arguments("color") == EnumValue("BLUE")
      )
    },
    testM("it forward args as correct type from variables") {
      case class QueryInput(color: COLOR, string: String)
      case class Query(query: Field => QueryInput => UIO[String])
      val query =
        """query MyQuery($color: COLOR!) {
          |  query(string: "test", color: $color)
          |}""".stripMargin

      for {
        ref         <- Ref.make[Option[Field]](None)
        api          = graphQL(
                         RootResolver(
                           Query(
                             query = info => { i =>
                               ref.set(Option(info)).as(i.string)
                             }
                           )
                         )
                       )
        interpreter <- api.interpreter
        _           <- interpreter.executeRequest(
                         request = GraphQLRequest(
                           query = Some(query),
                           // "color" is a string here since it will come directly from
                           // parsed JSON which is unaware that it should be an Enum
                           variables = Some(Map("color" -> Value.StringValue("BLUE")))
                         )
                       )
        res         <- ref.get
      } yield assertTrue(
        res.get.arguments("color") == EnumValue("BLUE")
      )
    },
    testM("it correctly handles lists of enums") {
      case class QueryInput(color: List[COLOR], string: String)
      case class Query(query: Field => QueryInput => UIO[String])
      val query =
        """query MyQuery($color: [COLOR!]!) {
          |  query(string: "test", color: $color)
          |}""".stripMargin

      for {
        ref         <- Ref.make[Option[Field]](None)
        api          = graphQL(
                         RootResolver(
                           Query(
                             query = info => { i =>
                               ref.set(Option(info)).as(i.string)
                             }
                           )
                         )
                       )
        interpreter <- api.interpreter
        _           <- interpreter.executeRequest(
                         request = GraphQLRequest(
                           query = Some(query),
                           // "color" is a string here since it will come directly from
                           // parsed JSON which is unaware that it should be an Enum
                           variables = Some(
                             Map(
                               "color" ->
                                 InputValue.ListValue(List(Value.StringValue("BLUE")))
                             )
                           )
                         )
                       )
        res         <- ref.get
      } yield assertTrue(
        res.get.arguments("color") == InputValue.ListValue(List(EnumValue("BLUE")))
      )
    },
    testM("it correctly handles objects of enums") {
      case class QueryInput(nested: QueryInputInput)
      case class QueryInputInput(color: List[COLOR], string: String)
      case class Query(query: Field => QueryInput => UIO[String])
      val query =
        """query MyQuery($color: [COLOR!]!) {
          |  query(nested: { color: $color, string: "foo" })
          |}""".stripMargin

      for {
        ref         <- Ref.make[Option[Field]](None)
        api          = graphQL(
                         RootResolver(
                           Query(
                             query = info => { i =>
                               ref.set(Option(info)).as(i.nested.string)
                             }
                           )
                         )
                       )
        interpreter <- api.interpreter
        _           <- interpreter.executeRequest(
                         request = GraphQLRequest(
                           query = Some(query),
                           // "color" is a string here since it will come directly from
                           // parsed JSON which is unaware that it should be an Enum
                           variables = Some(
                             Map(
                               "color" ->
                                 InputValue.ListValue(List(Value.StringValue("BLUE")))
                             )
                           )
                         )
                       )
        res         <- ref.get
      } yield assertTrue(
        res.get.arguments("nested") ==
          InputValue.ObjectValue(
            Map(
              "color"  -> InputValue.ListValue(List(EnumValue("BLUE"))),
              "string" -> Value.StringValue("foo")
            )
          )
      )
    },
    testM("it doesn't allow strings as enums in GQL syntax") {
      case class QueryInput(color: COLOR)
      case class Query(query: QueryInput => UIO[String])
      val query =
        """query {
          |  query(color: "BLUE")
          |}""".stripMargin

      val api = graphQL(
        RootResolver(
          Query(
            query = i => ZIO.succeed(i.toString)
          )
        )
      )

      for {
        interpreter <- api.interpreter
        res         <- interpreter.execute(query)
      } yield assert(res.errors.headOption)(isSome(anything))
    },
    testM("it correctly handles lists of objects with enums") {
      case class QueryInput(filter: List[Filter])
      case class Filter(color: COLOR)
      case class Query(query: QueryInput => String)
      val query =
        """query MyQuery($filter: [FilterInput!]!) {
          |  query(filter: $filter)
          |}""".stripMargin

      val api = graphQL(
        RootResolver(
          Query(
            query = q => q.filter.headOption.map(_.color.toString).getOrElse("Missing")
          )
        )
      )

      for {
        interpreter <- api.interpreter
        res         <- interpreter.executeRequest(
                         request = GraphQLRequest(
                           query = Some(query),
                           variables = Some(
                             Map(
                               "filter" ->
                                 InputValue.ListValue(
                                   List(
                                     InputValue.ObjectValue(
                                       Map("color" -> Value.StringValue("BLUE"))
                                     )
                                   )
                                 )
                             )
                           )
                         )
                       )
      } yield assertTrue(res.data.toString == "{\"query\":\"BLUE\"}")
    }
  )
}
