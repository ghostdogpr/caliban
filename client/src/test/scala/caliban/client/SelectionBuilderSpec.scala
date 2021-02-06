package caliban.client

import caliban.client.Operations.RootQuery
import caliban.client.Selection.Directive
import caliban.client.TestData._
import caliban.client.__Value.{ __ListValue, __ObjectValue, __StringValue }
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test._

object SelectionBuilderSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("SelectionBuilderSpec")(
      suite("query generation")(
        test("simple object") {
          val query  =
            Queries.characters() {
              Character.name
            }
          val (s, _) = SelectionBuilder.toGraphQL(query.toSelectionSet, useVariables = false)
          assert(s)(equalTo("characters{name}"))
        },
        test("combine 2 fields") {
          val query  =
            Queries.characters() {
              Character.name ~ Character.nicknames
            }
          val (s, _) = SelectionBuilder.toGraphQL(query.toSelectionSet, useVariables = false)
          assert(s)(equalTo("characters{name nicknames}"))
        },
        test("union type") {
          val query  =
            Queries.characters() {
              Character.name ~
                Character.nicknames ~
                Character
                  .role(Role.Captain.shipName, Role.Pilot.shipName, Role.Mechanic.shipName, Role.Engineer.shipName)
            }
          val (s, _) = SelectionBuilder.toGraphQL(query.toSelectionSet, useVariables = false)
          assert(s)(
            equalTo(
              "characters{name nicknames role{__typename ... on Captain{shipName} ... on Pilot{shipName} ... on Mechanic{shipName} ... on Engineer{shipName}}}"
            )
          )
        },
        test("argument") {
          val query  =
            Queries.characters(Some(Origin.MARS)) {
              Character.name
            }
          val (s, _) = SelectionBuilder.toGraphQL(query.toSelectionSet, useVariables = false)
          assert(s)(equalTo("""characters(origin:"MARS"){name}"""))
        },
        test("aliases") {
          val query  =
            Queries
              .character("Amos Burton") {
                Character.name
              }
              .copy(alias = Some("amos")) ~
              Queries
                .character("Naomi Nagata") {
                  Character.name
                }
                .copy(alias = Some("naomi"))
          val (s, _) = SelectionBuilder.toGraphQL(query.toSelectionSet, useVariables = false)
          assert(s)(equalTo("""amos:character(name:"Amos Burton"){name} naomi:character(name:"Naomi Nagata"){name}"""))
        },
        test("variables") {
          val query          =
            Queries
              .character("Amos Burton") {
                Character.name
              }
              .withAlias("amos") ~
              Queries
                .character("Naomi Nagata") {
                  Character.name
                }
                .withAlias("naomi")
          val (s, variables) = SelectionBuilder.toGraphQL(query.toSelectionSet, useVariables = true)
          assert(s)(equalTo("""amos:character(name:$name){name} naomi:character(name:$name1){name}""")) &&
          assert(variables.get("name"))(isSome(equalTo((__StringValue("Amos Burton"), "String!")))) &&
          assert(variables.get("name1"))(isSome(equalTo((__StringValue("Naomi Nagata"), "String!"))))
        },
        test("directives") {
          val query  =
            Queries
              .character("Amos Burton") {
                Character.name
              }
              .withDirective(Directive("yo", List(Argument("value", "what's up"))))
          val (s, _) = SelectionBuilder.toGraphQL(query.toSelectionSet, useVariables = false)
          assert(s)(equalTo("""character(name:"Amos Burton") @yo(value:"what's up"){name}"""))
        },
        test("directives + variables") {
          val query          =
            Queries
              .character("Amos Burton") {
                Character.name
              }
              .withDirective(Directive("yo", List(Argument("value", "what's up"))))
          val (s, variables) = SelectionBuilder.toGraphQL(query.toSelectionSet, useVariables = true)
          assert(s)(equalTo("""character(name:$name) @yo(value:$value){name}""")) &&
          assert(variables.get("name"))(isSome(equalTo((__StringValue("Amos Burton"), "String!")))) &&
          assert(variables.get("value"))(isSome(equalTo((__StringValue("what's up"), "String!"))))
        },
        test("query name") {
          val query = Queries.character("Amos Burton")(Character.name) toGraphQL (queryName = Some("GetCharacter"))
          assert(query.query)(equalTo("""query GetCharacter {character(name:"Amos Burton"){name}}"""))
        },
        test("pure fields") {
          val query = Queries.character("Amos Burton")(Character.name ~ SelectionBuilder.pure("Fake")).toGraphQL()
          assert(query.query)(equalTo("""query{character(name:"Amos Burton"){name}}"""))
        }
      ),
      suite("response parsing")(
        test("simple object") {
          val query    =
            Queries.characters() {
              Character.name
            }
          val response =
            __ObjectValue(List("characters" -> __ListValue(List(__ObjectValue(List("name" -> __StringValue("Amos")))))))
          assert(query.fromGraphQL(response))(isRight(equalTo(List("Amos"))))
        },
        test("combine 2 fields") {
          val query    =
            Queries.characters() {
              Character.name ~ Character.nicknames
            }
          val response =
            __ObjectValue(
              List(
                "characters" -> __ListValue(
                  List(
                    __ObjectValue(
                      List(
                        "name"      -> __StringValue("Amos Burton"),
                        "nicknames" -> __ListValue(List(__StringValue("Amos")))
                      )
                    )
                  )
                )
              )
            )
          assert(query.fromGraphQL(response))(isRight(equalTo(List("Amos Burton" -> List("Amos")))))
        },
        test("union type") {
          case class CharacterView(name: String, nicknames: List[String], role: Option[String])
          val query =
            Queries.characters() {
              (Character.name ~
                Character.nicknames ~
                Character
                  .role(Role.Captain.shipName, Role.Pilot.shipName, Role.Mechanic.shipName, Role.Engineer.shipName))
                .mapN(CharacterView)
            }

          val response =
            __ObjectValue(
              List(
                "characters" -> __ListValue(
                  List(
                    __ObjectValue(
                      List(
                        "name"      -> __StringValue("Amos Burton"),
                        "nicknames" -> __ListValue(List(__StringValue("Amos"))),
                        "role"      -> __ObjectValue(
                          List(
                            "__typename" -> __StringValue("Mechanic"),
                            "shipName"   -> __StringValue("Rocinante")
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          assert(query.fromGraphQL(response))(
            isRight(equalTo(List(CharacterView("Amos Burton", List("Amos"), Some("Rocinante")))))
          )
        },
        test("aliases") {
          val query    =
            Queries
              .character("Amos Burton") {
                Character.name
              }
              .copy(alias = Some("amos")) ~
              Queries
                .character("Naomi Nagata") {
                  Character.name
                }
                .copy(alias = Some("naomi"))
          val response =
            __ObjectValue(
              List(
                "amos"  -> __ObjectValue(List("name" -> __StringValue("Amos Burton"))),
                "naomi" -> __ObjectValue(List("name" -> __StringValue("Naomi Nagata")))
              )
            )
          assert(query.fromGraphQL(response))(isRight(equalTo((Some("Amos Burton"), Some("Naomi Nagata")))))
        },
        test("combineAll") {
          val query: SelectionBuilder[RootQuery, List[Option[String]]] = SelectionBuilder.combineAll(
            Queries.character("Amos Burton")(Character.name).copy(alias = Some("amos")),
            Queries.character("Naomi Nagata")(Character.name).copy(alias = Some("naomi"))
          )
          val response                                                 =
            __ObjectValue(
              List(
                "amos"  -> __ObjectValue(List("name" -> __StringValue("Amos Burton"))),
                "naomi" -> __ObjectValue(List("name" -> __StringValue("Naomi Nagata")))
              )
            )
          assert(query.fromGraphQL(response))(isRight(equalTo(List(Some("Amos Burton"), Some("Naomi Nagata")))))
        },
        test("pure") {
          val query    = Queries.character("Amos Burton")(Character.name) ~ SelectionBuilder.pure("Fake")
          val response = __ObjectValue(
            List("character" -> __ObjectValue(List("name" -> __StringValue("Amos Burton"))))
          )
          assert(query.fromGraphQL(response))(isRight(equalTo((Some("Amos Burton"), "Fake"))))
        },
        test("skip") {
          val query    = Queries.character("Amos Burton")(if (false) Character.name else SelectionBuilder.pure("Fake"))
          val response = __ObjectValue(
            List("character" -> __ObjectValue(List("name" -> __StringValue("Amos Burton"))))
          )
          assert(query.fromGraphQL(response))(isRight(equalTo(Some("Fake"))))
        }
      )
    )
}
