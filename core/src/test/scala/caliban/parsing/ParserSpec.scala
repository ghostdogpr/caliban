package caliban.parsing

import caliban.CalibanError.ParsingError
import caliban.InputValue
import caliban.InputValue._
import caliban.Value._
import caliban.parsing.ParserSpecUtils._
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition, TypeDefinition }
import caliban.parsing.adt.OperationType.{ Mutation, Query }
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.Type.{ FieldDefinition, ListType, NamedType }
import caliban.parsing.adt._
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
                      simpleField("name", index = 15),
                      simpleField("friends", selectionSet = List(simpleField("name", index = 73)), index = 57)
                    ),
                    index = 4
                  )
                ),
                sourceMapper = SourceMapper(query)
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
                    selectionSet = List(
                      simpleField("name", index = 28),
                      simpleField("height", arguments = Map("unit" -> EnumValue("FOOT")), index = 37)
                    ),
                    index = 4
                  )
                ),
                sourceMapper = SourceMapper(query)
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
                    selectionSet = List(simpleField("name", index = 44)),
                    index = 4
                  ),
                  simpleField(
                    "hero",
                    alias = Some("jediHero"),
                    arguments = Map("episode" -> EnumValue("JEDI")),
                    selectionSet = List(simpleField("name", index = 91)),
                    index = 55
                  )
                ),
                sourceMapper = SourceMapper(query)
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
                      "float" -> FloatValue("3.14"),
                      "bool"  -> BooleanValue(true),
                      "nope"  -> NullValue,
                      "enum"  -> EnumValue("YES"),
                      "list"  -> ListValue(List(IntValue(1), IntValue(2), IntValue(3))),
                      "obj"   -> ObjectValue(Map("name" -> StringValue("name")))
                    ),
                    selectionSet = List(simpleField("name", index = 131)),
                    index = 4
                  )
                ),
                sourceMapper = SourceMapper(query)
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
                    arguments = Map("message" -> StringValue("Hello,\n  World!\n\nYours,\n  GraphQL. ")),
                    index = 2
                  )
                ),
                sourceMapper = SourceMapper(query)
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
                      simpleField("id", index = 69),
                      simpleField("name", index = 76),
                      simpleField("profilePic", arguments = Map("size" -> VariableValue("devicePicSize")), index = 85)
                    ),
                    index = 51
                  )
                ),
                sourceMapper = SourceMapper(query)
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
                    directives = List(Directive("skip", Map("if" -> VariableValue("someTestM")), 57)),
                    index = 39
                  )
                ),
                sourceMapper = SourceMapper(query)
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
                selectionSet = List(simpleField("nothing", index = 50)),
                sourceMapper = SourceMapper(query)
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
                            selectionSet = List(FragmentSpread("friendFields", Nil)),
                            index = 42
                          ),
                          simpleField(
                            "mutualFriends",
                            arguments = Map("first" -> IntValue(10)),
                            selectionSet = List(FragmentSpread("friendFields", Nil)),
                            index = 95
                          )
                        ),
                        index = 24
                      )
                    )
                  ),
                  FragmentDefinition(
                    "friendFields",
                    NamedType("User", nonNull = false),
                    Nil,
                    List(
                      simpleField("id", index = 191),
                      simpleField("name", index = 196),
                      simpleField("profilePic", arguments = Map("size" -> IntValue(50)), index = 203)
                    )
                  )
                ),
                SourceMapper(query)
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
                      simpleField("handle", index = 77),
                      InlineFragment(
                        Some(NamedType("User", nonNull = false)),
                        Nil,
                        List(
                          simpleField("friends", selectionSet = List(simpleField("count", index = 126)), index = 108)
                        )
                      ),
                      InlineFragment(
                        Some(NamedType("Page", nonNull = false)),
                        Nil,
                        List(simpleField("likers", selectionSet = List(simpleField("count", index = 187)), index = 170))
                      )
                    ),
                    index = 31
                  )
                ),
                sourceMapper = SourceMapper(query)
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
                      simpleField("id", index = 82),
                      simpleField("name", index = 89),
                      InlineFragment(
                        None,
                        List(Directive("include", Map("if" -> VariableValue("expandedInfo")), 102)),
                        List(
                          simpleField("firstName", index = 138),
                          simpleField("lastName", index = 154),
                          simpleField("birthday", index = 169)
                        )
                      )
                    ),
                    index = 55
                  )
                ),
                sourceMapper = SourceMapper(query)
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
                          simpleField("story", selectionSet = List(simpleField("likeCount", index = 59)), index = 45)
                        ),
                        index = 13
                      )
                    )
                  )
                ),
                SourceMapper(query)
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
          assertM(
            Parser.parseQuery(query).run,
            fails(equalTo(ParsingError("Position 4:3, found \"}\\n}\"", locationInfo = Some(LocationInfo(3, 4)))))
          )
        },
        testM("type") {
          val gqltype =
            """type Hero {
              |"name desc" name(pad: Int!): String! @skip(if: $someTestM)
              |"nick desc" nick: String!
              |bday: Int
              |suits: [String]
              |powers: [String!]!
              |}""".stripMargin
          //planetSavedTimesCmp(moreThan: Int!)!: Int!
          assertM(
            Parser.parseQuery(gqltype),
            equalTo(
              Document(
                List(
                  TypeDefinition(
                    "Hero",
                    List(
                      FieldDefinition(
                        Some("name desc"),
                        "name",
                        List("pad" -> NamedType("Int", true)),
                        NamedType("String", true),
                        List(Directive("skip", Map("if" -> VariableValue("someTestM")), index = 49))
                      ),
                      FieldDefinition(Some("nick desc"), "nick", List(), NamedType("String", true), List()),
                      FieldDefinition(None, "bday", List(), NamedType("Int", false), List()),
                      FieldDefinition(None, "suits", List(), ListType(NamedType("String", false), false), List()),
                      FieldDefinition(None, "powers", List(), ListType(NamedType("String", true), true), List())
                    )
                  )
                ),
                sourceMapper = SourceMapper.apply(gqltype)
              )
            )
          )
        }
      )
    )

object ParserSpecUtils {

  def simpleQuery(
    name: Option[String] = None,
    variableDefinitions: List[VariableDefinition] = Nil,
    directives: List[Directive] = Nil,
    selectionSet: List[Selection] = Nil,
    sourceMapper: SourceMapper = SourceMapper.empty
  ) = Document(List(OperationDefinition(Query, name, variableDefinitions, directives, selectionSet)), sourceMapper)

  def simpleField(
    name: String,
    alias: Option[String] = None,
    arguments: Map[String, InputValue] = Map(),
    directives: List[Directive] = Nil,
    selectionSet: List[Selection] = Nil,
    index: Int = 0
  ) = Field(alias, name, arguments, directives, selectionSet, index)
}
