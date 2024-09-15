package caliban.federation.v2x

import caliban.InputValue.{ ListValue, ObjectValue }
import caliban.Macros.gqldoc
import caliban.TestUtils._
import caliban.Value.StringValue
import caliban.parsing.Parser
import caliban.parsing.adt.{ Definition, Directive }
import caliban.schema.Schema.auto._
import caliban._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.circe.JsoniterScalaCodec._
import io.circe.Json
import zio.ZIO
import zio.test.Assertion.hasSameElements
import zio.test._

object FederationV2Spec extends ZIOSpecDefault {
  override def spec =
    suite("FederationV2Spec")(
      test("includes schema directives - v2.0") {
        import caliban.federation.v2_0._
        makeSchemaDirectives(federated(_)).map { schemaDirectives =>
          val linkDirective = schemaDirectives.find(_.name == "link")
          val url           = linkDirective.flatMap(_.arguments.get("url"))
          val imports       = linkDirective.toList
            .flatMap(_.arguments.get("import"))
            .collect { case ListValue(values) =>
              values.collect { case StringValue(value) => value }
            }
            .flatten

          assertTrue(
            linkDirective.isDefined,
            url.get == StringValue("https://specs.apollo.dev/federation/v2.0")
          ) && assert(imports)(
            hasSameElements(
              "@key" ::
                "@requires" ::
                "@provides" ::
                "@external" ::
                "@shareable" ::
                "@tag" ::
                "@inaccessible" ::
                "@override" ::
                "@extends" :: Nil
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
              ),
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
              )
            )
          )
        }
      },
      test("introspection doesn't contain _FieldSet scalar") {
        import caliban.federation.v2_8._
        val interpreter = (graphQL(resolver) @@ federated).interpreter
        val query       = gqldoc("""{ __schema { types { name } } }""")
        interpreter
          .flatMap(_.execute(query))
          .map(d =>
            ResponseValue.at(
              PathValue.Key("__schema") :: PathValue.Key("types") :: PathValue.Key("name") :: Nil
            )(d.data)
          )
          .map(responseValue =>
            assertTrue(
              !responseValue
                .is(_.subtype[ResponseValue.ListValue])
                .values
                .contains(StringValue("_Any")),
              !responseValue
                .is(_.subtype[ResponseValue.ListValue])
                .values
                .contains(StringValue("_FieldSet"))
            )
          )
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
      data        <- interpreter.execute(query).flatMap(resp => ZIO.attempt(readFromString[Json](resp.data.toString)))
      sdl         <- ZIO.fromEither(data.hcursor.downField("_service").downField("sdl").as[String])
      document    <- ZIO.fromEither(Parser.parseQuery(sdl))
    } yield document.definitions.flatMap {
      case Definition.TypeSystemDefinition.SchemaDefinition(d, _, _, _, _) =>
        d.map(_.copy(index = 0)) // Unset the index to make the test deterministic
      case _ => Nil
    }
  }
}
