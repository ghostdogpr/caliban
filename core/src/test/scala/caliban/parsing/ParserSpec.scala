package caliban.parsing

import caliban.CalibanError.ParsingError
import caliban.InputValue
import caliban.InputValue._
import caliban.Value._
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension.SchemaExtension
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension._
import caliban.parsing.adt.OperationType.{ Mutation, Query }
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.Type.{ ListType, NamedType }
import caliban.parsing.adt._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object ParserSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
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
        assertM(Parser.parseQuery(query))(
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
        assertM(Parser.parseQuery(query))(
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
        assertM(Parser.parseQuery(query))(
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
        assertM(Parser.parseQuery(query))(
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
        assertM(Parser.parseQuery(query))(
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
        assertM(Parser.parseQuery(query))(
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
        assertM(Parser.parseQuery(query))(
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
        assertM(Parser.parseQuery(query))(
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
        assertM(Parser.parseQuery(query))(
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
        assertM(Parser.parseQuery(query))(
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
        assertM(Parser.parseQuery(query))(
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
        assertM(Parser.parseQuery(query))(
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
        assertM(Parser.parseQuery(query).run)(
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
        assertM(Parser.parseQuery(gqltype))(
          equalTo(
            Document(
              List(
                ObjectTypeDefinition(
                  None,
                  "Hero",
                  Nil,
                  Nil,
                  List(
                    FieldDefinition(
                      Some("name desc"),
                      "name",
                      List(InputValueDefinition(None, "pad", NamedType("Int", true), None, Nil)),
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
      },
      testM("extend schema with directives") {
        val gqlSchemaExtension = "extend schema @addedDirective"
        assertM(Parser.parseQuery(gqlSchemaExtension))(
          equalTo(
            Document(
              List(
                SchemaExtension(
                  List(Directive("addedDirective", index = 14)),
                  None,
                  None,
                  None
                )
              ),
              sourceMapper = SourceMapper.apply(gqlSchemaExtension)
            )
          )
        )
      },
      testM("extend schema with directives and operations") {
        val gqlSchemaExtension =
          """
            |extend schema @addedDirective {
            | query: Query
            | mutation: Mutation
            | subscription: Subscription
            |}
            |""".stripMargin
        assertM(Parser.parseQuery(gqlSchemaExtension))(
          equalTo(
            Document(
              List(
                SchemaExtension(
                  List(Directive("addedDirective", index = 15)),
                  Some("Query"),
                  Some("Mutation"),
                  Some("Subscription")
                )
              ),
              sourceMapper = SourceMapper.apply(gqlSchemaExtension)
            )
          )
        )
      },
      testM("extend schema with operations") {
        val gqlSchemaExtension =
          """
            |extend schema {
            | query: Query
            | mutation: Mutation
            | subscription: Subscription
            |}
            |""".stripMargin
        assertM(Parser.parseQuery(gqlSchemaExtension))(
          equalTo(
            Document(
              List(
                SchemaExtension(
                  Nil,
                  Some("Query"),
                  Some("Mutation"),
                  Some("Subscription")
                )
              ),
              sourceMapper = SourceMapper.apply(gqlSchemaExtension)
            )
          )
        )
      },
      testM("extend scalar with directives") {
        val gqlScalarExtension = "extend scalar SomeScalar @foo(arg0: $someTestM)"
        assertM(Parser.parseQuery(gqlScalarExtension))(
          equalTo(
            Document(
              List(
                ScalarTypeExtension(
                  "SomeScalar",
                  List(Directive("foo", Map("arg0" -> VariableValue("someTestM")), index = 25))
                )
              ),
              sourceMapper = SourceMapper.apply(gqlScalarExtension)
            )
          )
        )
      },
      testM("extend type with interfaces") {
        val gqlTypeExtension = "extend type Hero implements SomeInterface"
        assertM(Parser.parseQuery(gqlTypeExtension))(
          equalTo(
            Document(
              List(
                ObjectTypeExtension(
                  "Hero",
                  List(NamedType("SomeInterface", false)),
                  Nil,
                  Nil
                )
              ),
              sourceMapper = SourceMapper.apply(gqlTypeExtension)
            )
          )
        )
      },
      testM("extend type with interfaces and fields") {
        val gqlTypeExtension =
          """extend type Hero implements SomeInterface {
            |"name desc" name(pad: Int!): String! @skip(if: $someTestM)
            |"nick desc" nick: String!
            |bday: Int
            |suits: [String]
            |powers: [String!]!
            |}""".stripMargin
        assertM(Parser.parseQuery(gqlTypeExtension))(
          equalTo(
            Document(
              List(
                ObjectTypeExtension(
                  "Hero",
                  List(NamedType("SomeInterface", false)),
                  Nil,
                  List(
                    FieldDefinition(
                      Some("name desc"),
                      "name",
                      List(InputValueDefinition(None, "pad", NamedType("Int", true), None, Nil)),
                      NamedType("String", true),
                      List(Directive("skip", Map("if" -> VariableValue("someTestM")), index = 81))
                    ),
                    FieldDefinition(Some("nick desc"), "nick", List(), NamedType("String", true), List()),
                    FieldDefinition(None, "bday", List(), NamedType("Int", false), List()),
                    FieldDefinition(None, "suits", List(), ListType(NamedType("String", false), false), List()),
                    FieldDefinition(None, "powers", List(), ListType(NamedType("String", true), true), List())
                  )
                )
              ),
              sourceMapper = SourceMapper.apply(gqlTypeExtension)
            )
          )
        )
      },
      testM("extend type with interfaces and directives") {
        val gqlTypeExtension = "extend type Hero implements SomeInterface @addedDirective"
        assertM(Parser.parseQuery(gqlTypeExtension))(
          equalTo(
            Document(
              List(
                ObjectTypeExtension(
                  "Hero",
                  List(NamedType("SomeInterface", false)),
                  List(Directive("addedDirective", index = 42)),
                  Nil
                )
              ),
              sourceMapper = SourceMapper.apply(gqlTypeExtension)
            )
          )
        )
      },
      testM("extend type with interfaces, directives and fields") {
        val gqlTypeExtension =
          """extend type Hero implements SomeInterface @addedDirective {
            |"name desc" name(pad: Int!): String! @skip(if: $someTestM)
            |"nick desc" nick: String!
            |bday: Int
            |suits: [String]
            |powers: [String!]!
            |}""".stripMargin
        assertM(Parser.parseQuery(gqlTypeExtension))(
          equalTo(
            Document(
              List(
                ObjectTypeExtension(
                  "Hero",
                  List(NamedType("SomeInterface", false)),
                  List(Directive("addedDirective", index = 42)),
                  List(
                    FieldDefinition(
                      Some("name desc"),
                      "name",
                      List(InputValueDefinition(None, "pad", NamedType("Int", true), None, Nil)),
                      NamedType("String", true),
                      List(Directive("skip", Map("if" -> VariableValue("someTestM")), index = 97))
                    ),
                    FieldDefinition(Some("nick desc"), "nick", List(), NamedType("String", true), List()),
                    FieldDefinition(None, "bday", List(), NamedType("Int", false), List()),
                    FieldDefinition(None, "suits", List(), ListType(NamedType("String", false), false), List()),
                    FieldDefinition(None, "powers", List(), ListType(NamedType("String", true), true), List())
                  )
                )
              ),
              sourceMapper = SourceMapper.apply(gqlTypeExtension)
            )
          )
        )
      },
      testM("extend type with directives") {
        val gqlTypeExtension = "extend type Hero @addedDirective"
        assertM(Parser.parseQuery(gqlTypeExtension))(
          equalTo(
            Document(
              List(
                ObjectTypeExtension(
                  "Hero",
                  Nil,
                  List(Directive("addedDirective", index = 17)),
                  Nil
                )
              ),
              sourceMapper = SourceMapper.apply(gqlTypeExtension)
            )
          )
        )
      },
      testM("extend type with directives and fields") {
        val gqlTypeExtension =
          """extend type Hero @addedDirective {
            |"name desc" name(pad: Int!): String! @skip(if: $someTestM)
            |"nick desc" nick: String!
            |bday: Int
            |suits: [String]
            |powers: [String!]!
            |}""".stripMargin
        assertM(Parser.parseQuery(gqlTypeExtension))(
          equalTo(
            Document(
              List(
                ObjectTypeExtension(
                  "Hero",
                  Nil,
                  List(Directive("addedDirective", index = 17)),
                  List(
                    FieldDefinition(
                      Some("name desc"),
                      "name",
                      List(InputValueDefinition(None, "pad", NamedType("Int", true), None, Nil)),
                      NamedType("String", true),
                      List(Directive("skip", Map("if" -> VariableValue("someTestM")), index = 72))
                    ),
                    FieldDefinition(Some("nick desc"), "nick", List(), NamedType("String", true), List()),
                    FieldDefinition(None, "bday", List(), NamedType("Int", false), List()),
                    FieldDefinition(None, "suits", List(), ListType(NamedType("String", false), false), List()),
                    FieldDefinition(None, "powers", List(), ListType(NamedType("String", true), true), List())
                  )
                )
              ),
              sourceMapper = SourceMapper.apply(gqlTypeExtension)
            )
          )
        )
      },
      testM("extend type with fields") {
        val gqlTypeExtension =
          """extend type Hero {
            |"name desc" name(pad: Int!): String! @skip(if: $someTestM)
            |"nick desc" nick: String!
            |bday: Int
            |suits: [String]
            |powers: [String!]!
            |}""".stripMargin
        assertM(Parser.parseQuery(gqlTypeExtension))(
          equalTo(
            Document(
              List(
                ObjectTypeExtension(
                  "Hero",
                  Nil,
                  Nil,
                  List(
                    FieldDefinition(
                      Some("name desc"),
                      "name",
                      List(InputValueDefinition(None, "pad", NamedType("Int", true), None, Nil)),
                      NamedType("String", true),
                      List(Directive("skip", Map("if" -> VariableValue("someTestM")), index = 56))
                    ),
                    FieldDefinition(Some("nick desc"), "nick", List(), NamedType("String", true), List()),
                    FieldDefinition(None, "bday", List(), NamedType("Int", false), List()),
                    FieldDefinition(None, "suits", List(), ListType(NamedType("String", false), false), List()),
                    FieldDefinition(None, "powers", List(), ListType(NamedType("String", true), true), List())
                  )
                )
              ),
              sourceMapper = SourceMapper.apply(gqlTypeExtension)
            )
          )
        )
      },
      testM("extend interface with directives") {
        val gqlInterfaceExtension = "extend interface NamedEntity @addedDirective"
        assertM(Parser.parseQuery(gqlInterfaceExtension))(
          equalTo(
            Document(
              List(
                InterfaceTypeExtension(
                  "NamedEntity",
                  List(Directive("addedDirective", index = 29)),
                  Nil
                )
              ),
              sourceMapper = SourceMapper.apply(gqlInterfaceExtension)
            )
          )
        )
      },
      testM("extend interface with directives and fields") {
        val gqlInterfaceExtension =
          """
            |extend interface NamedEntity @addedDirective {
            |  nickname: String
            |}
            |""".stripMargin
        assertM(Parser.parseQuery(gqlInterfaceExtension))(
          equalTo(
            Document(
              List(
                InterfaceTypeExtension(
                  "NamedEntity",
                  List(Directive("addedDirective", index = 30)),
                  List(FieldDefinition(None, "nickname", Nil, NamedType("String", false), Nil))
                )
              ),
              sourceMapper = SourceMapper.apply(gqlInterfaceExtension)
            )
          )
        )
      },
      testM("extend interface with fields") {
        val gqlInterfaceExtension =
          """
            |extend interface NamedEntity {
            |  nickname: String
            |}
            |""".stripMargin
        assertM(Parser.parseQuery(gqlInterfaceExtension))(
          equalTo(
            Document(
              List(
                InterfaceTypeExtension(
                  "NamedEntity",
                  Nil,
                  List(FieldDefinition(None, "nickname", Nil, NamedType("String", false), Nil))
                )
              ),
              sourceMapper = SourceMapper.apply(gqlInterfaceExtension)
            )
          )
        )
      },
      testM("extend union with directives") {
        val gqlUnionExtension = "extend union SearchResult @addedDirective"
        assertM(Parser.parseQuery(gqlUnionExtension))(
          equalTo(
            Document(
              List(
                UnionTypeExtension(
                  "SearchResult",
                  List(Directive("addedDirective", index = 26)),
                  Nil
                )
              ),
              sourceMapper = SourceMapper.apply(gqlUnionExtension)
            )
          )
        )
      },
      testM("extend union with directives and union members") {
        val gqlUnionExtension = "extend union SearchResult @addedDirective = Photo | Person"
        assertM(Parser.parseQuery(gqlUnionExtension))(
          equalTo(
            Document(
              List(
                UnionTypeExtension(
                  "SearchResult",
                  List(Directive("addedDirective", index = 26)),
                  List("Photo", "Person")
                )
              ),
              sourceMapper = SourceMapper.apply(gqlUnionExtension)
            )
          )
        )
      },
      testM("extend union with union members") {
        val gqlUnionExtension = "extend union SearchResult = Photo | Person"
        assertM(Parser.parseQuery(gqlUnionExtension))(
          equalTo(
            Document(
              List(
                UnionTypeExtension(
                  "SearchResult",
                  Nil,
                  List("Photo", "Person")
                )
              ),
              sourceMapper = SourceMapper.apply(gqlUnionExtension)
            )
          )
        )
      },
      testM("extend enum with directives") {
        val gqlEnumExtension = "extend enum Direction @addedDirective"
        assertM(Parser.parseQuery(gqlEnumExtension))(
          equalTo(
            Document(
              List(
                EnumTypeExtension(
                  "Direction",
                  List(Directive("addedDirective", index = 22)),
                  Nil
                )
              ),
              sourceMapper = SourceMapper.apply(gqlEnumExtension)
            )
          )
        )
      },
      testM("extend enum with directives and values") {
        val gqlEnumExtension =
          """
            |extend enum Direction @addedDirective {
            |  NORTH_WEST
            |  NORTH_EAST
            |  SOUTH_WEST
            |  SOUTH_EAST
            |}
            |""".stripMargin
        assertM(Parser.parseQuery(gqlEnumExtension))(
          equalTo(
            Document(
              List(
                EnumTypeExtension(
                  "Direction",
                  List(Directive("addedDirective", index = 23)),
                  List(
                    EnumValueDefinition(None, "NORTH_WEST", Nil),
                    EnumValueDefinition(None, "NORTH_EAST", Nil),
                    EnumValueDefinition(None, "SOUTH_WEST", Nil),
                    EnumValueDefinition(None, "SOUTH_EAST", Nil)
                  )
                )
              ),
              sourceMapper = SourceMapper.apply(gqlEnumExtension)
            )
          )
        )
      },
      testM("extend enum with values") {
        val gqlEnumExtension =
          """
            |extend enum Direction {
            |  NORTH_WEST
            |  NORTH_EAST
            |  SOUTH_WEST
            |  SOUTH_EAST
            |}
            |""".stripMargin
        assertM(Parser.parseQuery(gqlEnumExtension))(
          equalTo(
            Document(
              List(
                EnumTypeExtension(
                  "Direction",
                  Nil,
                  List(
                    EnumValueDefinition(None, "NORTH_WEST", Nil),
                    EnumValueDefinition(None, "NORTH_EAST", Nil),
                    EnumValueDefinition(None, "SOUTH_WEST", Nil),
                    EnumValueDefinition(None, "SOUTH_EAST", Nil)
                  )
                )
              ),
              sourceMapper = SourceMapper.apply(gqlEnumExtension)
            )
          )
        )
      },
      testM("extend input with directives") {
        val gqlInputExtension = "extend input Point @addedDirective"
        assertM(Parser.parseQuery(gqlInputExtension))(
          equalTo(
            Document(
              List(
                InputObjectTypeExtension(
                  "Point",
                  List(Directive("addedDirective", index = 19)),
                  Nil
                )
              ),
              sourceMapper = SourceMapper.apply(gqlInputExtension)
            )
          )
        )
      },
      testM("extend input with directives and fields") {
        val gqlInputExtension =
          """
            |extend input Point @addedDirective {
            |  z: Int!
            |}
            |""".stripMargin
        assertM(Parser.parseQuery(gqlInputExtension))(
          equalTo(
            Document(
              List(
                InputObjectTypeExtension(
                  "Point",
                  List(Directive("addedDirective", index = 20)),
                  List(
                    InputValueDefinition(None, "z", NamedType("Int", nonNull = true), None, Nil)
                  )
                )
              ),
              sourceMapper = SourceMapper.apply(gqlInputExtension)
            )
          )
        )
      },
      testM("extend input with fields") {
        val gqlInputExtension =
          """
            |extend input Point {
            |  z: Int!
            |}
            |""".stripMargin
        assertM(Parser.parseQuery(gqlInputExtension))(
          equalTo(
            Document(
              List(
                InputObjectTypeExtension(
                  "Point",
                  Nil,
                  List(
                    InputValueDefinition(None, "z", NamedType("Int", nonNull = true), None, Nil)
                  )
                )
              ),
              sourceMapper = SourceMapper.apply(gqlInputExtension)
            )
          )
        )
      }
    )

  def simpleQuery(
    name: Option[String] = None,
    variableDefinitions: List[VariableDefinition] = Nil,
    directives: List[Directive] = Nil,
    selectionSet: List[Selection] = Nil,
    sourceMapper: SourceMapper = SourceMapper.empty
  ): Document =
    Document(List(OperationDefinition(Query, name, variableDefinitions, directives, selectionSet)), sourceMapper)

  def simpleField(
    name: String,
    alias: Option[String] = None,
    arguments: Map[String, InputValue] = Map(),
    directives: List[Directive] = Nil,
    selectionSet: List[Selection] = Nil,
    index: Int = 0
  ): Field = Field(alias, name, arguments, directives, selectionSet, index)
}
