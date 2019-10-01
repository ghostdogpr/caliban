package caliban.parsing

import caliban.CalibanError.ParsingError
import caliban.parsing.ParserSpecUtils._
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.OperationType.{ Mutation, Query }
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt.Value._
import caliban.parsing.adt.{ Directive, Document, Selection, Value, VariableDefinition }
import zio.test.Assertion._
import zio.test._

object ParserSpec
    extends DefaultRunnableSpec(
      suite("ParserSpec")(
        testM("simple query with fields") {
          val query = """{
                        |  hero {
                        |    name
                        |    # Queries can have comments!
                        |    friends {
                        |      name
                        |    }
                        |  }
                        |}""".stripMargin
          assertM(
            Parser.parseQuery(query),
            equalTo(
              simpleQuery(
                selectionSet = List(
                  simpleField(
                    "hero",
                    selectionSet = List(
                      simpleField("name"),
                      simpleField("friends", selectionSet = List(simpleField("name")))
                    )
                  )
                )
              )
            )
          )
        },
        testM("arguments") {
          val query = """{
                        |  human(id: "1000") {
                        |    name
                        |    height(unit: FOOT)
                        |  }
                        |}""".stripMargin
          assertM(
            Parser.parseQuery(query),
            equalTo(
              simpleQuery(
                selectionSet = List(
                  simpleField(
                    "human",
                    arguments = Map("id" -> StringValue("1000")),
                    selectionSet =
                      List(simpleField("name"), simpleField("height", arguments = Map("unit" -> EnumValue("FOOT"))))
                  )
                )
              )
            )
          )
        },
        testM("aliases") {
          val query = """{
                        |  empireHero: hero(episode: EMPIRE) {
                        |    name
                        |  }
                        |  jediHero: hero(episode: JEDI) {
                        |    name
                        |  }
                        |}""".stripMargin
          assertM(
            Parser.parseQuery(query),
            equalTo(
              simpleQuery(
                selectionSet = List(
                  simpleField(
                    "hero",
                    alias = Some("empireHero"),
                    arguments = Map("episode" -> EnumValue("EMPIRE")),
                    selectionSet = List(simpleField("name"))
                  ),
                  simpleField(
                    "hero",
                    alias = Some("jediHero"),
                    arguments = Map("episode" -> EnumValue("JEDI")),
                    selectionSet = List(simpleField("name"))
                  )
                )
              )
            )
          )
        },
        testM("input values") {
          val query = """{
                        |  human(id: "1000", int: 3, float: 3.14, bool: true, nope: null, enum: YES, list: [1,2,3], obj: {
                        |   name: "name"
                        |   }
                        |  ) {
                        |    name
                        |  }
                        |}""".stripMargin
          assertM(
            Parser.parseQuery(query),
            equalTo(
              simpleQuery(
                selectionSet = List(
                  simpleField(
                    "human",
                    arguments = Map(
                      "id"    -> StringValue("1000"),
                      "int"   -> IntValue(3),
                      "float" -> FloatValue(3.14f),
                      "bool"  -> BooleanValue(true),
                      "nope"  -> NullValue,
                      "enum"  -> EnumValue("YES"),
                      "list"  -> ListValue(List(IntValue(1), IntValue(2), IntValue(3))),
                      "obj"   -> ObjectValue(Map("name" -> StringValue("name")))
                    ),
                    selectionSet = List(simpleField("name"))
                  )
                )
              )
            )
          )
        },
        testM("block strings") {
          val query = "{ sendEmail(message: \"\"\"\n  Hello,\n    World!\n\n  Yours,\n    GraphQL. \"\"\") }"
          assertM(
            Parser.parseQuery(query),
            equalTo(
              simpleQuery(
                selectionSet = List(
                  simpleField(
                    "sendEmail",
                    arguments = Map("message" -> StringValue("Hello,\n  World!\n\nYours,\n  GraphQL. "))
                  )
                )
              )
            )
          )
        },
        testM("variables") {
          val query = """query getZuckProfile($devicePicSize: Int = 60) {
                        |  user(id: 4) {
                        |    id
                        |    name
                        |    profilePic(size: $devicePicSize)
                        |  }
                        |}""".stripMargin
          assertM(
            Parser.parseQuery(query),
            equalTo(
              simpleQuery(
                name = Some("getZuckProfile"),
                variableDefinitions = List(
                  VariableDefinition("devicePicSize", NamedType("Int", nonNull = false), Some(IntValue(60)), Nil)
                ),
                selectionSet = List(
                  simpleField(
                    "user",
                    arguments = Map("id" -> IntValue(4)),
                    selectionSet = List(
                      simpleField("id"),
                      simpleField("name"),
                      simpleField("profilePic", arguments = Map("size" -> VariableValue("devicePicSize")))
                    )
                  )
                )
              )
            )
          )
        },
        testM("directives") {
          val query = """query myQuery($someTestM: Boolean) {
                        |  experimentalField @skip(if: $someTestM)
                        |}""".stripMargin
          assertM(
            Parser.parseQuery(query),
            equalTo(
              simpleQuery(
                name = Some("myQuery"),
                variableDefinitions =
                  List(VariableDefinition("someTestM", NamedType("Boolean", nonNull = false), None, Nil)),
                selectionSet = List(
                  simpleField(
                    "experimentalField",
                    directives = List(Directive("skip", Map("if" -> VariableValue("someTestM"))))
                  )
                )
              )
            )
          )
        },
        testM("list and non-null types") {
          val query = """query getZuckProfile($devicePicSize: [Int!]!) {
                        |  nothing
                        |}""".stripMargin
          assertM(
            Parser.parseQuery(query),
            equalTo(
              simpleQuery(
                name = Some("getZuckProfile"),
                variableDefinitions = List(
                  VariableDefinition(
                    "devicePicSize",
                    ListType(NamedType("Int", nonNull = true), nonNull = true),
                    None,
                    Nil
                  )
                ),
                selectionSet = List(simpleField("nothing"))
              )
            )
          )
        },
        testM("fragments") {
          val query = """query withFragments {
                        |  user(id: 4) {
                        |    friends(first: 10) {
                        |      ...friendFields
                        |    }
                        |    mutualFriends(first: 10) {
                        |      ...friendFields
                        |    }
                        |  }
                        |}
                        |
                        |fragment friendFields on User {
                        |  id
                        |  name
                        |  profilePic(size: 50)
                        |}""".stripMargin
          assertM(
            Parser.parseQuery(query),
            equalTo(
              Document(
                List(
                  OperationDefinition(
                    Query,
                    Some("withFragments"),
                    Nil,
                    Nil,
                    List(
                      simpleField(
                        "user",
                        arguments = Map("id" -> IntValue(4)),
                        selectionSet = List(
                          simpleField(
                            "friends",
                            arguments = Map("first" -> IntValue(10)),
                            selectionSet = List(FragmentSpread("friendFields", Nil))
                          ),
                          simpleField(
                            "mutualFriends",
                            arguments = Map("first" -> IntValue(10)),
                            selectionSet = List(FragmentSpread("friendFields", Nil))
                          )
                        )
                      )
                    )
                  ),
                  FragmentDefinition(
                    "friendFields",
                    NamedType("User", nonNull = false),
                    Nil,
                    List(
                      simpleField("id"),
                      simpleField("name"),
                      simpleField("profilePic", arguments = Map("size" -> IntValue(50)))
                    )
                  )
                )
              )
            )
          )
        },
        testM("inline fragments") {
          val query = """query inlineFragmentTyping {
                        |  profiles(handles: ["zuck", "cocacola"]) {
                        |    handle
                        |    ... on User {
                        |      friends {
                        |        count
                        |      }
                        |    }
                        |    ... on Page {
                        |      likers {
                        |        count
                        |      }
                        |    }
                        |  }
                        |}""".stripMargin
          assertM(
            Parser.parseQuery(query),
            equalTo(
              simpleQuery(
                name = Some("inlineFragmentTyping"),
                selectionSet = List(
                  simpleField(
                    "profiles",
                    arguments = Map("handles" -> ListValue(List(StringValue("zuck"), StringValue("cocacola")))),
                    selectionSet = List(
                      simpleField("handle"),
                      InlineFragment(
                        Some(NamedType("User", nonNull = false)),
                        Nil,
                        List(simpleField("friends", selectionSet = List(simpleField("count"))))
                      ),
                      InlineFragment(
                        Some(NamedType("Page", nonNull = false)),
                        Nil,
                        List(simpleField("likers", selectionSet = List(simpleField("count"))))
                      )
                    )
                  )
                )
              )
            )
          )
        },
        testM("inline fragments with directives") {
          val query = """query inlineFragmentNoType($expandedInfo: Boolean) {
                        |  user(handle: "zuck") {
                        |    id
                        |    name
                        |    ... @include(if: $expandedInfo) {
                        |      firstName
                        |      lastName
                        |      birthday
                        |    }
                        |  }
                        |}""".stripMargin
          assertM(
            Parser.parseQuery(query),
            equalTo(
              simpleQuery(
                name = Some("inlineFragmentNoType"),
                variableDefinitions =
                  List(VariableDefinition("expandedInfo", NamedType("Boolean", nonNull = false), None, Nil)),
                selectionSet = List(
                  simpleField(
                    "user",
                    arguments = Map("handle" -> StringValue("zuck")),
                    selectionSet = List(
                      simpleField("id"),
                      simpleField("name"),
                      InlineFragment(
                        None,
                        List(Directive("include", Map("if" -> VariableValue("expandedInfo")))),
                        List(
                          simpleField("firstName"),
                          simpleField("lastName"),
                          simpleField("birthday")
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        },
        testM("mutation") {
          val query = """mutation {
                        |  likeStory(storyID: 12345) {
                        |    story {
                        |      likeCount
                        |    }
                        |  }
                        |}""".stripMargin
          assertM(
            Parser.parseQuery(query),
            equalTo(
              Document(
                List(
                  OperationDefinition(
                    Mutation,
                    None,
                    Nil,
                    Nil,
                    List(
                      simpleField(
                        "likeStory",
                        arguments = Map("storyID" -> IntValue(12345)),
                        selectionSet = List(
                          simpleField("story", selectionSet = List(simpleField("likeCount")))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        },
        testM("invalid syntax") {
          val query = """{
                        |  hero {
                        |    name(
                        |  }
                        |}""".stripMargin
          assertM(Parser.parseQuery(query).run, fails(equalTo(ParsingError("Position 4:3, found \"}\\n}\""))))
        }
      )
    )

object ParserSpecUtils {

  def simpleQuery(
    name: Option[String] = None,
    variableDefinitions: List[VariableDefinition] = Nil,
    directives: List[Directive] = Nil,
    selectionSet: List[Selection] = Nil
  ) = Document(List(OperationDefinition(Query, name, variableDefinitions, directives, selectionSet)))

  def simpleField(
    name: String,
    alias: Option[String] = None,
    arguments: Map[String, Value] = Map(),
    directives: List[Directive] = Nil,
    selectionSet: List[Selection] = Nil
  ) = Field(alias, name, arguments, directives, selectionSet)
}
