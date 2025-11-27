package caliban.client

import caliban.client.FieldBuilder._
import caliban.client.Operations.RootQuery
import caliban.client.Selection.Directive
import caliban.client.SelectionBuilder.Field
import caliban.client.TestData._
import caliban.client.__Value.{ __ListValue, __ObjectValue, __StringValue }
import zio.test.Assertion._
import zio.test._

object SelectionBuilderSpec extends ZIOSpecDefault {

  override def spec =
    suite("SelectionBuilderSpec")(
      suite("query generation")(
        test("simple object") {
          val query  =
            Queries.characters() {
              Character.name
            }
          val (s, _) = SelectionBuilder.toGraphQL(query.toSelectionSet, useVariables = false)
          assertTrue(s == "{characters{name}}")
        },
        test("combine 2 fields") {
          val query  =
            Queries.characters() {
              Character.name ~ Character.nicknames
            }
          val (s, _) = SelectionBuilder.toGraphQL(query.toSelectionSet, useVariables = false)
          assertTrue(s == "{characters{name nicknames}}")
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
          assertTrue(
            s == "{characters{name nicknames role{__typename ... on Captain{shipName} ... on Pilot{shipName} ... on Mechanic{shipName} ... on Engineer{shipName}}}}"
          )
        },
        test("union type optional") {
          val query  =
            Queries.characters() {
              Character.name ~
                Character.nicknames ~
                Character.roleOption(onCaptain = Some(Role.Captain.shipName))
            }
          val (s, _) = SelectionBuilder.toGraphQL(query.toSelectionSet, useVariables = false)
          assertTrue(s == "{characters{name nicknames role{__typename ... on Captain{shipName}}}}")
        },
        test("argument") {
          val query  =
            Queries.characters(Some(Origin.MARS)) {
              Character.name
            }
          val (s, _) = SelectionBuilder.toGraphQL(query.toSelectionSet, useVariables = false)
          assertTrue(s == """{characters(origin:"MARS"){name}}""")
        },
        test("union type with optional parameters") {
          case class CharacterView(name: String, nicknames: List[String], role: Option[Option[String]])
          val query =
            Queries.characters() {
              (Character.name ~
                Character.nicknames ~
                Character.roleOption(onMechanic = Some(Role.Mechanic.shipName))).mapN(CharacterView(_, _, _))
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
            isRight(equalTo(List(CharacterView("Amos Burton", List("Amos"), Some(Some("Rocinante"))))))
          )
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
          assertTrue(s == """{amos:character(name:"Amos Burton"){name} naomi:character(name:"Naomi Nagata"){name}}""")
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
          assertTrue(
            s == """{amos:character(name:$name){name} naomi:character(name:$name1){name}}""",
            variables("name") == ((__StringValue("Amos Burton"), "String!")),
            variables("name1") == ((__StringValue("Naomi Nagata"), "String!"))
          )
        },
        test("directives") {
          val query  =
            Queries
              .character("Amos Burton") {
                Character.name
              }
              .withDirective(Directive("yo", List(Argument("value", "what's up", "String!"))))
          val (s, _) = SelectionBuilder.toGraphQL(query.toSelectionSet, useVariables = false)
          assertTrue(s == """{character(name:"Amos Burton") @yo(value:"what's up"){name}}""")
        },
        test("directives + variables") {
          val query          =
            Queries
              .character("Amos Burton") {
                Character.name
              }
              .withDirective(Directive("yo", List(Argument("value", "what's up", "String!"))))
          val (s, variables) = SelectionBuilder.toGraphQL(query.toSelectionSet, useVariables = true)
          assertTrue(
            s == """{character(name:$name) @yo(value:$value){name}}""",
            variables("name") == ((__StringValue("Amos Burton"), "String!")),
            variables("value") == ((__StringValue("what's up"), "String!"))
          )
        },
        test("query name") {
          val query = Queries.character("Amos Burton")(Character.name).toGraphQL(queryName = Some("GetCharacter"))
          assertTrue(query.query == """query GetCharacter{character(name:"Amos Burton"){name}}""")
        },
        test("query name + directives") {
          val GraphQLRequest(s, variables) = Queries
            .character("Amos Burton")(Character.name)
            .toGraphQL(
              queryName = Some("GetCharacter"),
              directives = List(Directive("yo", List(Argument("value", "what's up", "String!")))),
              useVariables = false
            )
          assertTrue(s == """query GetCharacter @yo(value:"what's up"){character(name:"Amos Burton"){name}}""")
        },
        test("query name + directives + variables") {
          val GraphQLRequest(s, variables) = Queries
            .character("Amos Burton")(Character.name)
            .toGraphQL(
              queryName = Some("GetCharacter"),
              directives = List(Directive("yo", List(Argument("value", "what's up", "String!")))),
              useVariables = true
            )
          assertTrue(
            s == """query GetCharacter ($name: String!,$value: String!) @yo(value:$value){character(name:"Amos Burton"){name}}"""
          )
          assertTrue(variables("name") == __StringValue("Amos Burton")) &&
          assertTrue(variables("value") == __StringValue("what's up"))
        },
        test("pure fields") {
          val query = Queries.character("Amos Burton")(Character.name ~ SelectionBuilder.pure("Fake")).toGraphQL()
          assertTrue(query.query == """query{character(name:"Amos Burton"){name}}""")
        }
      ),
      suite("fragments")(
        test("simple fragment") {
          val query = Queries
            .character("Amos Burton")(
              SelectionBuilder.fragment("CF", "Character")(Character.name ~ Character.nicknames)
            )
            .toGraphQL()

          assertTrue(
            query.query == """query{character(name:"Amos Burton"){...CF}}fragment CF on Character{name nicknames}"""
          )
        },
        test("fragment deduplicates") {
          val fragment = SelectionBuilder.fragment("CF", "Character")(Character.name ~ Character.nicknames)
          val query    = (Queries
            .character("Amos Burton")(fragment)
            .withAlias("amos") ~ Queries.character("Naomi Nagata")(fragment).withAlias("naomi"))
            .toGraphQL()

          assertTrue(
            query.query == """query{amos:character(name:"Amos Burton"){...CF} naomi:character(name:"Naomi Nagata"){...CF}}fragment CF on Character{name nicknames}"""
          )
        },
        test("structural equality of fragments") {
          val fragment1 = SelectionBuilder.fragment("CF", "Character")(Character.name ~ Character.nicknames)
          val fragment2 = SelectionBuilder.fragment("CF", "Character")(Character.name ~ Character.nicknames)
          val query     = (Queries.character("Amos Burton")(fragment1).withAlias("amos") ~
            Queries.character("Naomi Nagata")(fragment2).withAlias("naomi"))
            .toGraphQL()
          assertTrue(
            query.query == """query{amos:character(name:"Amos Burton"){...CF} naomi:character(name:"Naomi Nagata"){...CF}}fragment CF on Character{name nicknames}"""
          )
        },
        test("distinct fragments") {
          val fragment1 = SelectionBuilder.fragment("CF1", "Character")(Character.nicknames)
          val fragment2 = SelectionBuilder.fragment("CF2", "Character")(Character.name)
          val query     = (Queries.character("Amos Burton")(fragment1).withAlias("amos") ~
            Queries.character("Naomi Nagata")(fragment2).withAlias("naomi"))
            .toGraphQL()
          assertTrue(
            query.query == """query{amos:character(name:"Amos Burton"){...CF1} naomi:character(name:"Naomi Nagata"){...CF2}}fragment CF1 on Character{nicknames} fragment CF2 on Character{name}"""
          )
        },
        test("nested fragments") {
          val characterFragment = SelectionBuilder.fragment("CF", "Character")(
            Character.name ~ SelectionBuilder.fragment("Inner", "Character")(
              Character.roleOption(
                onCaptain = Some(SelectionBuilder.fragment("CaptainFrag", "Captain")(Role.Captain.shipName))
              )
            )
          )

          val query = Queries.character("Amos Burton")(characterFragment).toGraphQL()
          assertTrue(
            query.query == """query{character(name:"Amos Burton"){...CF}}fragment CF on Character{name ...Inner} fragment Inner on Character{role{__typename ... on Captain{...CaptainFrag}}} fragment CaptainFrag on Captain{shipName}"""
          )
        },
        test("fragments with variable references") {
          val characterFragment = SelectionBuilder.fragment("CF", "Character")(
            Character.name ~
              Character.roleOption(onCaptain = Some(Role.Captain.shipName))
          )

          val query = SelectionBuilder
            .fragment("QueryFrag", "Query")(Queries.character("Amos Burton")(characterFragment))
            .toGraphQL(
              queryName = Some("GetCharacter"),
              useVariables = true
            )

          assertTrue(
            query.query == """query GetCharacter($name: String!){...QueryFrag}fragment QueryFrag on Query{character(name:$name){...CF}} fragment CF on Character{name role{__typename ... on Captain{shipName}}}""",
            query.variables == Map("name" -> __Value.__StringValue("Amos Burton"))
          )
        },
        test("fragments with directives") {
          assertCompletes
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
                .mapN(CharacterView(_, _, _))
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
        },
        test("drop null values in input object") {
          import caliban.client.__Value._

          case class CharacterInput(name: String, description: Option[String], nicknames: List[String] = Nil)
          object CharacterInput {
            implicit val encoder: ArgEncoder[CharacterInput] = (value: CharacterInput) =>
              __ObjectValue(
                List(
                  "name"        -> implicitly[ArgEncoder[String]].encode(value.name),
                  "description" -> implicitly[ArgEncoder[Option[String]]].encode(value.description),
                  "nicknames"   -> __ListValue(value.nicknames.map(value => implicitly[ArgEncoder[String]].encode(value)))
                )
              )
          }

          object Query {
            def addCharacter[A](
              character: CharacterInput
            )(sel: SelectionBuilder[Character, A])(implicit
              encoder: ArgEncoder[CharacterInput]
            ): Field[RootQuery, Option[A]] =
              Field(
                "addCharacter",
                OptionOf(Obj(sel)),
                arguments = List(Argument("character", character, "CharacterInput!")(encoder))
              )
          }

          val query = Query.addCharacter(CharacterInput("name", None, Nil))(Character.name)

          assertTrue(
            query
              .toGraphQL(dropNullInputValues = true)
              .query == """query{addCharacter(character:{name:"name",nicknames:[]}){name}}"""
          )
        },
        test("drop null values in input object by explicit ArgEncoder") {
          import caliban.client.__Value._

          case class CharacterInput(name: String, description: Option[String], nicknames: List[String] = Nil)
          object CharacterInput {
            implicit val encoder: ArgEncoder[CharacterInput] = (value: CharacterInput) =>
              __ObjectValue(
                List(
                  "name"        -> implicitly[ArgEncoder[String]].encode(value.name),
                  "description" -> implicitly[ArgEncoder[Option[String]]].encode(value.description),
                  "nicknames"   -> __ListValue(value.nicknames.map(value => implicitly[ArgEncoder[String]].encode(value)))
                )
              )
          }

          object Query {
            def addCharacter[A](
              character: CharacterInput
            )(sel: SelectionBuilder[Character, A])(implicit
              encoder: ArgEncoder[CharacterInput]
            ): Field[RootQuery, Option[A]] =
              Field(
                "addCharacter",
                OptionOf(Obj(sel)),
                arguments = List(Argument("character", character, "CharacterInput!")(encoder))
              )
          }

          implicit val encoderWithoutNull: ArgEncoder[CharacterInput] =
            CharacterInput.encoder.dropNullValues

          val query = Query.addCharacter(CharacterInput("name", None, Nil))(Character.name)

          assertTrue(query.toGraphQL().query == """query{addCharacter(character:{name:"name",nicknames:[]}){name}}""")
        },
        test("interface common fields and subtype fields combination") {
          import caliban.client.FieldBuilder._
          import caliban.client._

          type Order = String
          object Order {
            def name: SelectionBuilder[Order, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
          }

          type Ascending = String
          object Ascending {
            def name: SelectionBuilder[Ascending, String] =
              _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
          }

          type Sort = String
          object Sort {
            def orderOption[A](
              onAscending: Option[SelectionBuilder[Ascending, A]] = None
            ): SelectionBuilder[Sort, Option[Option[A]]] = _root_.caliban.client.SelectionBuilder.Field(
              "order",
              OptionOf(
                ChoiceOf(
                  Map("Ascending" -> onAscending.fold[FieldBuilder[Option[A]]](NullField)(a => OptionOf(Obj(a))))
                )
              )
            )
            def orderInterface[A](order: SelectionBuilder[Order, A]): SelectionBuilder[Sort, Option[A]] =
              _root_.caliban.client.SelectionBuilder.Field("order", OptionOf(Obj(order)))
          }

          object Query {
            def sort[A](sel: SelectionBuilder[Sort, A]): Field[RootQuery, A] =
              Field("sort", Obj(sel))
          }

          val selection = Sort.orderInterface(Order.name).withAlias("common") ~
            Sort.orderOption(Some(Ascending.name)).withAlias("subtype")
          assertTrue(
            Query
              .sort(selection)
              .toGraphQL()
              .query == "query{sort{common:order{name} subtype:order{__typename ... on Ascending{name}}}}"
          )
        }
      )
    )
}
