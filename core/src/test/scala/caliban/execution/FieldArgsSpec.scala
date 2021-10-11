package caliban.execution

import zio._

import caliban.CalibanError
import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.InputValue
import caliban.Value.EnumValue
import caliban.RootResolver
import caliban.schema.Annotations.GQLDefault
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment
import caliban.Value
import caliban.GraphQLRequest

object FieldArgsSpec extends DefaultRunnableSpec {
  sealed trait COLOR
  object COLOR {
    case object GREEN extends COLOR
    case object BLUE  extends COLOR
  }

  override def spec: Spec[Environment,TestFailure[Any],TestSuccess] = suite("FieldArgsSpec")(
    testM("it forward args of correct type") {
      final case class QueryInput(color: COLOR, string: String)
      final case class Query(query: Field => QueryInput => UIO[String])
      val query =
        """query{
          |  query(string: "test", color: BLUE)
          |}""".stripMargin

      for {
        ref         <- Ref.make[Option[Field]](None)
        api          = graphQL(
                         RootResolver(
                           Query(
                             query = info =>
                               i =>
                                 ref.set(Option(info)) *>
                                   ZIO.succeed(i.string)
                           )
                         )
                       )
        interpreter <- api.interpreter
        _           <- interpreter.execute(query)

        res <- ref.get
      } yield assertTrue(
        res.get.arguments.get("color").get == EnumValue("BLUE")
      )
    }
  ) +
    testM("it forward args as correct type from variables") {
      final case class QueryInput(color: COLOR, string: String)
      final case class Query(query: Field => QueryInput => UIO[String])
      val query =
        """query MyQuery($color: COLOR!) {
          |  query(string: "test", color: $color)
          |}""".stripMargin

      for {
        ref         <- Ref.make[Option[Field]](None)
        api          = graphQL(
                         RootResolver(
                           Query(
                             query = info => {
                               i =>
                                 ref.set(Option(info)) *>
                                   ZIO.succeed(i.string)
                             }
                           )
                         )
                       )
        interpreter <- api.interpreter
        qres        <- interpreter.executeRequest(
                         request = GraphQLRequest(
                           query = Some(query),
                           // "color" is a string here since it will come directly from
                           // parsed JSON which is unaware that it should be an Enum
                           variables = Some(Map("color" -> Value.StringValue("BLUE")))
                         ),
                         skipValidation = false,
                         enableIntrospection = true,
                         queryExecution = QueryExecution.Parallel
                       )
        res         <- ref.get
      } yield assertTrue(
        res.get.arguments.get("color").get == EnumValue("BLUE")
      )
    }
}
