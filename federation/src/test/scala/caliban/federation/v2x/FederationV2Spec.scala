package caliban.federation.v2x

import caliban.InputValue.{ ListValue, ObjectValue }
import caliban.Macros.gqldoc
import caliban.Value.StringValue
import caliban.parsing.Parser
import caliban.parsing.adt.{ Definition, Directive }
import caliban.schema.Schema.auto._
import caliban._
import io.circe.Json
import io.circe.parser.decode
import zio.ZIO
import zio.test.{ assertTrue, ZIOSpecDefault }

object FederationV2Spec extends ZIOSpecDefault {
  override def spec =
    suite("FederationV2Spec")(
      test("includes schema directives - v2.0") {
        import caliban.federation.v2_0._
        makeSchemaDirectives(federated(_)).map { schemaDirectives =>
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
                        StringValue("@override"),
                        StringValue("@extends")
                      )
                    )
                  )
                )
              )
          )
        }
      },
      test("includes schema directives - v2.1") {
        import caliban.federation.v2_1._
        makeSchemaDirectives(federated(_)).map { schemaDirectives =>
          assertTrue(
            schemaDirectives
              .contains(
                Directive(
                  name = "link",
                  Map(
                    "url"    -> StringValue("https://specs.apollo.dev/federation/v2.1"),
                    "import" -> ListValue(
                      List(
                        StringValue("@key"),
                        StringValue("@requires"),
                        StringValue("@provides"),
                        StringValue("@external"),
                        StringValue("@shareable"),
                        StringValue("@tag"),
                        StringValue("@inaccessible"),
                        StringValue("@override"),
                        StringValue("@extends"),
                        StringValue("@composeDirective")
                      )
                    )
                  )
                )
              )
          )
        }
      },
      test("includes schema directives - v2.3") {
        import caliban.federation.v2_3._
        makeSchemaDirectives(federated(_)).map { schemaDirectives =>
          assertTrue(
            schemaDirectives
              .contains(
                Directive(
                  name = "link",
                  Map(
                    "url"    -> StringValue("https://specs.apollo.dev/federation/v2.3"),
                    "import" -> ListValue(
                      List(
                        StringValue("@key"),
                        StringValue("@requires"),
                        StringValue("@provides"),
                        StringValue("@external"),
                        StringValue("@shareable"),
                        StringValue("@tag"),
                        StringValue("@inaccessible"),
                        StringValue("@override"),
                        StringValue("@extends"),
                        StringValue("@composeDirective"),
                        StringValue("@interfaceObject")
                      )
                    )
                  )
                )
              )
          )
        }
      },
      test("includes schema directives - custom") {
        object myFederation
            extends FederationV2(
              Versions.v2_3 ::
                Link(
                  "https://myspecs.dev/myDirective/v1.0",
                  List(
                    Import("@myDirective"),
                    Import("@anotherDirective", as = Some("@hello"))
                  )
                ) ::
                ComposeDirective("@myDirective") ::
                ComposeDirective("@hello") :: Nil
            )
            with FederationDirectivesV2_3

        // Then import your new federation object instead of `caliban.federation.v2_3`
        import myFederation._
        makeSchemaDirectives(federated(_)).map { schemaDirectives =>
          assertTrue(
            schemaDirectives == List(
              Directive(
                "link",
                Map(
                  "url"    -> StringValue("https://specs.apollo.dev/federation/v2.3"),
                  "import" -> ListValue(
                    List(
                      StringValue("@key"),
                      StringValue("@requires"),
                      StringValue("@provides"),
                      StringValue("@external"),
                      StringValue("@shareable"),
                      StringValue("@tag"),
                      StringValue("@inaccessible"),
                      StringValue("@override"),
                      StringValue("@extends"),
                      StringValue("@composeDirective"),
                      StringValue("@interfaceObject")
                    )
                  )
                )
              ),
              Directive(
                "link",
                Map(
                  "url"    -> StringValue("https://myspecs.dev/myDirective/v1.0"),
                  "import" -> ListValue(
                    List(
                      StringValue("@myDirective"),
                      ObjectValue(
                        Map(
                          "name" -> StringValue("@anotherDirective"),
                          "as"   -> StringValue("@hello")
                        )
                      )
                    )
                  )
                )
              ),
              Directive(
                "composeDirective",
                Map(
                  "name" -> StringValue("@myDirective")
                )
              ),
              Directive(
                "composeDirective",
                Map(
                  "name" -> StringValue("@hello")
                )
              )
            )
          )
        }
      }
    )

  private def makeSchemaDirectives(f: GraphQL[Any] => GraphQL[Any]) = {
    case class Query(
      hello: String
    )

    val api = graphQL(
      RootResolver(
        Query(hello = "Hello World!")
      )
    )

    val query = gqldoc("query { _service { sdl } }")

    for {
      interpreter <- f(api).interpreter
      data        <- interpreter.execute(query).map(resp => decode[Json](resp.data.toString)).absolve
      sdl         <- ZIO.fromEither(data.hcursor.downField("_service").downField("sdl").as[String])
      document    <- Parser.parseQuery(sdl)
    } yield document.definitions.flatMap {
      case Definition.TypeSystemDefinition.SchemaDefinition(d, _, _, _) =>
        d.map(_.copy(index = 0)) // Unset the index to make the test deterministic
      case _                                                            => Nil
    }
  }
}
