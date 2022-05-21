package caliban.federation.v2
import caliban.InputValue.ListValue
import caliban.Macros.gqldoc
import caliban.Value.StringValue
import caliban.{ GraphQL, RootResolver }
import caliban.parsing.Parser
import caliban.parsing.adt.{ Definition, Directive }
import io.circe.Json
import zio.test.{ assertTrue, DefaultRunnableSpec }
import io.circe.syntax._
import io.circe.parser.decode
import zio.IO

object FederationV2Spec extends DefaultRunnableSpec {
  override def spec =
    suite("FederationV2Spec")(
      test("includes schema directives") {
        case class Query(
          hello: String
        )

        val api = GraphQL.graphQL(
          RootResolver(
            Query(hello = "Hello World!")
          )
        ) @@ federated

        val query = gqldoc("query { _service { sdl } }")

        for {
          interpreter <- api.interpreter
          data        <- interpreter.execute(query).map(resp => decode[Json](resp.data.toString)).absolve
          sdl         <- IO.fromEither(data.hcursor.downField("_service").downField("sdl").as[String])
          document    <- Parser.parseQuery(sdl)
        } yield {
          val schemaDirectives =
            document.definitions.flatMap {
              case Definition.TypeSystemDefinition.SchemaDefinition(d, _, _, _) =>
                d.map(_.copy(index = 0)) // Unset the index to make the test deterministic
              case _ => Nil
            }

          assertTrue(
            schemaDirectives
              .contains(
                Directive(
                  name = "link",
                  Map(
                    "url"    -> StringValue("https://specs.apollo.dev/federation/v2.0"),
                    "import" -> ListValue(
                      List(
                        StringValue("@key"),
                        StringValue("@requires"),
                        StringValue("@provides"),
                        StringValue("@external"),
                        StringValue("@shareable"),
                        StringValue("@tag"),
                        StringValue("@inaccessible"),
                        StringValue("@override")
                      )
                    )
                  )
                )
              )
          )
        }

      }
    )
}
