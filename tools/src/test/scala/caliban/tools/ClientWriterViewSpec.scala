package caliban.tools

import caliban.parsing.Parser
import caliban.tools.implicits.ScalarMappings
import zio.RIO
import zio.blocking.Blocking
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object ClientWriterViewSpec extends DefaultRunnableSpec {

  val gen: String => RIO[Blocking, String] = (schema: String) =>
    Parser
      .parseQuery(schema)
      .flatMap(doc => Formatter.format(ClientWriter.write(doc, genView = true)(ScalarMappings(None)).head._2, None))

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("ClientWriterViewSpec")(
      testM("simple object type") {
        val schema =
          """
             type Character {
               name: String!
               age: Int!
               nicknames: [String!]!
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {

    final case class CharacterView(name: String, age: Int, nicknames: List[String])

    type ViewSelection = SelectionBuilder[Character, CharacterView]

    def view: ViewSelection = (name ~ age ~ nicknames).map { case ((name, age), nicknames) =>
      CharacterView(name, age, nicknames)
    }

    def name: SelectionBuilder[Character, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def age: SelectionBuilder[Character, Int]                = _root_.caliban.client.SelectionBuilder.Field("age", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
  }

}
"""
          )
        )
      },
      testM("nested object type") {
        val schema =
          """
             type Q {
               users: [User!]!
             }
             
             type Character {
               name: String!
               age: Int!
               nicknames(arg: Int): [String!]!
             }

             type User {
               characters(name: String!): [Character!]!
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Q
  object Q {

    final case class QView[UsersSelection](users: List[UsersSelection])

    type ViewSelection[UsersSelection] = SelectionBuilder[Q, QView[UsersSelection]]

    def view[UsersSelection](usersSelection: SelectionBuilder[User, UsersSelection]): ViewSelection[UsersSelection] =
      users(usersSelection).map(users => QView(users))

    def users[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Q, List[A]] =
      _root_.caliban.client.SelectionBuilder.Field("users", ListOf(Obj(innerSelection)))
  }

  type Character
  object Character {

    final case class CharacterView(name: String, age: Int, nicknames: List[String])

    type ViewSelection = SelectionBuilder[Character, CharacterView]

    def view(nicknamesArg: Option[Int] = None): ViewSelection = (name ~ age ~ nicknames(nicknamesArg)).map {
      case ((name, age), nicknames) => CharacterView(name, age, nicknames)
    }

    def name: SelectionBuilder[Character, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def age: SelectionBuilder[Character, Int]     = _root_.caliban.client.SelectionBuilder.Field("age", Scalar())
    def nicknames(arg: Option[Int] = None)(implicit
      encoder0: ArgEncoder[Option[Int]]
    ): SelectionBuilder[Character, List[String]] = _root_.caliban.client.SelectionBuilder
      .Field("nicknames", ListOf(Scalar()), arguments = List(Argument("arg", arg, "Int")(encoder0)))
  }

  type User
  object User {

    final case class UserView[CharactersSelection](characters: List[CharactersSelection])

    type ViewSelection[CharactersSelection] = SelectionBuilder[User, UserView[CharactersSelection]]

    def view[CharactersSelection](
      charactersName: String
    )(charactersSelection: SelectionBuilder[Character, CharactersSelection]): ViewSelection[CharactersSelection] =
      characters(charactersName)(charactersSelection).map(characters => UserView(characters))

    def characters[A](name: String)(
      innerSelection: SelectionBuilder[Character, A]
    )(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, List[A]] = _root_.caliban.client.SelectionBuilder
      .Field("characters", ListOf(Obj(innerSelection)), arguments = List(Argument("name", name, "String!")(encoder0)))
  }

}
"""
          )
        )
      },
      testM("recursive object type") {
        val schema =
          """
             type Character {
               name: String!
               age: Int!
               friends(filter: String): [Character!]!
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {

    final case class CharacterView[FriendsSelection](name: String, age: Int, friends: List[FriendsSelection])

    type ViewSelection[FriendsSelection] = SelectionBuilder[Character, CharacterView[FriendsSelection]]

    def view[FriendsSelection](friendsFilter: Option[String] = None)(
      friendsSelection: SelectionBuilder[Character, FriendsSelection]
    ): ViewSelection[FriendsSelection] = (name ~ age ~ friends(friendsFilter)(friendsSelection)).map {
      case ((name, age), friends) => CharacterView(name, age, friends)
    }

    def name: SelectionBuilder[Character, String] = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def age: SelectionBuilder[Character, Int]     = _root_.caliban.client.SelectionBuilder.Field("age", Scalar())
    def friends[A](filter: Option[String] = None)(innerSelection: SelectionBuilder[Character, A])(implicit
      encoder0: ArgEncoder[Option[String]]
    ): SelectionBuilder[Character, List[A]] = _root_.caliban.client.SelectionBuilder
      .Field("friends", ListOf(Obj(innerSelection)), arguments = List(Argument("filter", filter, "String")(encoder0)))
  }

}
"""
          )
        )
      },
      testM("generic view for Option[List[Option[A]] types") {
        val schema =
          """
            type ProjectMember {
              id: Int
              name: String
            }

            type ProjectMemberEdge {
              cursor: String!
              node: ProjectMember
            }

            type PageInfo {
              endCursor: String
              hasNextPage: Boolean!
              hasPreviousPage: Boolean!
              startCursor: String
            }

            type ProjectMemberConnection {
              edges: [ProjectMemberEdge]
              nodes: [ProjectMember]
              pageInfo: PageInfo!
            }
            """

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type ProjectMember
  object ProjectMember {

    final case class ProjectMemberView(id: Option[Int], name: Option[String])

    type ViewSelection = SelectionBuilder[ProjectMember, ProjectMemberView]

    def view: ViewSelection = (id ~ name).map { case (id, name) => ProjectMemberView(id, name) }

    def id: SelectionBuilder[ProjectMember, Option[Int]]      =
      _root_.caliban.client.SelectionBuilder.Field("id", OptionOf(Scalar()))
    def name: SelectionBuilder[ProjectMember, Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("name", OptionOf(Scalar()))
  }

  type ProjectMemberEdge
  object ProjectMemberEdge {

    final case class ProjectMemberEdgeView[NodeSelection](cursor: String, node: Option[NodeSelection])

    type ViewSelection[NodeSelection] = SelectionBuilder[ProjectMemberEdge, ProjectMemberEdgeView[NodeSelection]]

    def view[NodeSelection](
      nodeSelection: SelectionBuilder[ProjectMember, NodeSelection]
    ): ViewSelection[NodeSelection] = (cursor ~ node(nodeSelection)).map { case (cursor, node) =>
      ProjectMemberEdgeView(cursor, node)
    }

    def cursor: SelectionBuilder[ProjectMemberEdge, String]                                                         =
      _root_.caliban.client.SelectionBuilder.Field("cursor", Scalar())
    def node[A](innerSelection: SelectionBuilder[ProjectMember, A]): SelectionBuilder[ProjectMemberEdge, Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field("node", OptionOf(Obj(innerSelection)))
  }

  type PageInfo
  object PageInfo {

    final case class PageInfoView(
      endCursor: Option[String],
      hasNextPage: Boolean,
      hasPreviousPage: Boolean,
      startCursor: Option[String]
    )

    type ViewSelection = SelectionBuilder[PageInfo, PageInfoView]

    def view: ViewSelection = (endCursor ~ hasNextPage ~ hasPreviousPage ~ startCursor).map {
      case (((endCursor, hasNextPage), hasPreviousPage), startCursor) =>
        PageInfoView(endCursor, hasNextPage, hasPreviousPage, startCursor)
    }

    def endCursor: SelectionBuilder[PageInfo, Option[String]]   =
      _root_.caliban.client.SelectionBuilder.Field("endCursor", OptionOf(Scalar()))
    def hasNextPage: SelectionBuilder[PageInfo, Boolean]        =
      _root_.caliban.client.SelectionBuilder.Field("hasNextPage", Scalar())
    def hasPreviousPage: SelectionBuilder[PageInfo, Boolean]    =
      _root_.caliban.client.SelectionBuilder.Field("hasPreviousPage", Scalar())
    def startCursor: SelectionBuilder[PageInfo, Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("startCursor", OptionOf(Scalar()))
  }

  type ProjectMemberConnection
  object ProjectMemberConnection {

    final case class ProjectMemberConnectionView[EdgesSelection, NodesSelection, PageInfoSelection](
      edges: Option[List[Option[EdgesSelection]]],
      nodes: Option[List[Option[NodesSelection]]],
      pageInfo: PageInfoSelection
    )

    type ViewSelection[EdgesSelection, NodesSelection, PageInfoSelection] = SelectionBuilder[
      ProjectMemberConnection,
      ProjectMemberConnectionView[EdgesSelection, NodesSelection, PageInfoSelection]
    ]

    def view[EdgesSelection, NodesSelection, PageInfoSelection](
      edgesSelection: SelectionBuilder[ProjectMemberEdge, EdgesSelection],
      nodesSelection: SelectionBuilder[ProjectMember, NodesSelection],
      pageInfoSelection: SelectionBuilder[PageInfo, PageInfoSelection]
    ): ViewSelection[EdgesSelection, NodesSelection, PageInfoSelection] =
      (edges(edgesSelection) ~ nodes(nodesSelection) ~ pageInfo(pageInfoSelection)).map {
        case ((edges, nodes), pageInfo) => ProjectMemberConnectionView(edges, nodes, pageInfo)
      }

    def edges[A](
      innerSelection: SelectionBuilder[ProjectMemberEdge, A]
    ): SelectionBuilder[ProjectMemberConnection, Option[List[Option[A]]]] =
      _root_.caliban.client.SelectionBuilder.Field("edges", OptionOf(ListOf(OptionOf(Obj(innerSelection)))))
    def nodes[A](
      innerSelection: SelectionBuilder[ProjectMember, A]
    ): SelectionBuilder[ProjectMemberConnection, Option[List[Option[A]]]] =
      _root_.caliban.client.SelectionBuilder.Field("nodes", OptionOf(ListOf(OptionOf(Obj(innerSelection)))))
    def pageInfo[A](innerSelection: SelectionBuilder[PageInfo, A]): SelectionBuilder[ProjectMemberConnection, A] =
      _root_.caliban.client.SelectionBuilder.Field("pageInfo", Obj(innerSelection))
  }

}
"""
          )
        )
      },
      testM("generic view for scala keywords") {
        val schema =
          """
          type package {
            name: String
          }

          type match {
            package: package
            version: String
          }
            """

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type `package`
  object `package` {

    final case class packageView(name: Option[String])

    type ViewSelection = SelectionBuilder[`package`, packageView]

    def view: ViewSelection = name.map(name => packageView(name))

    def name: SelectionBuilder[`package`, Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("name", OptionOf(Scalar()))
  }

  type `match`
  object `match` {

    final case class matchView[PackageSelection](`package`: Option[PackageSelection], version: Option[String])

    type ViewSelection[PackageSelection] = SelectionBuilder[`match`, matchView[PackageSelection]]

    def view[PackageSelection](
      packageSelection: SelectionBuilder[`package`, PackageSelection]
    ): ViewSelection[PackageSelection] = (`package`(packageSelection) ~ version).map { case (package$, version) =>
      matchView(package$, version)
    }

    def `package`[A](innerSelection: SelectionBuilder[`package`, A]): SelectionBuilder[`match`, Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field("package", OptionOf(Obj(innerSelection)))
    def version: SelectionBuilder[`match`, Option[String]]                                                 =
      _root_.caliban.client.SelectionBuilder.Field("version", OptionOf(Scalar()))
  }

}
"""
          )
        )
      },
      testM("union case") {
        val schema =
          """
          type Character {
              name: String!
              nicknames: [String!]!
              role: Role
          }

          union Role = Captain | Pilot
          type Captain {
              shipName: String!
          }
          type Pilot {
              shipName: String!
          }
            """

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {

    final case class CharacterView[RoleSelection](name: String, nicknames: List[String], role: Option[RoleSelection])

    type ViewSelection[RoleSelection] = SelectionBuilder[Character, CharacterView[RoleSelection]]

    def view[RoleSelection](
      roleSelectionOnCaptain: SelectionBuilder[Captain, RoleSelection],
      roleSelectionOnPilot: SelectionBuilder[Pilot, RoleSelection]
    ): ViewSelection[RoleSelection] = (name ~ nicknames ~ role(roleSelectionOnCaptain, roleSelectionOnPilot)).map {
      case ((name, nicknames), role) => CharacterView(name, nicknames, role)
    }

    def name: SelectionBuilder[Character, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
    def role[A](
      onCaptain: SelectionBuilder[Captain, A],
      onPilot: SelectionBuilder[Pilot, A]
    ): SelectionBuilder[Character, Option[A]] = _root_.caliban.client.SelectionBuilder
      .Field("role", OptionOf(ChoiceOf(Map("Captain" -> Obj(onCaptain), "Pilot" -> Obj(onPilot)))))
  }

  type Captain
  object Captain {

    final case class CaptainView(shipName: String)

    type ViewSelection = SelectionBuilder[Captain, CaptainView]

    def view: ViewSelection = shipName.map(shipName => CaptainView(shipName))

    def shipName: SelectionBuilder[Captain, String] = _root_.caliban.client.SelectionBuilder.Field("shipName", Scalar())
  }

  type Pilot
  object Pilot {

    final case class PilotView(shipName: String)

    type ViewSelection = SelectionBuilder[Pilot, PilotView]

    def view: ViewSelection = shipName.map(shipName => PilotView(shipName))

    def shipName: SelectionBuilder[Pilot, String] = _root_.caliban.client.SelectionBuilder.Field("shipName", Scalar())
  }

}
"""
          )
        )
      },
      testM("type with more than 22 fields / function args / selection args") {
        val schema =
          """
             type Big {
               user1: User!
               user2: User!
               user3: User!
               user4: User!
               user5: User!
               user6: User!
               user7: User!
               user8: User!
               user9: User!
               user10: User!
               user11: User!
               user12: User!
               user13: User!
               user14: User!
               user15: User!
               user16: User!
               user17: User!
               user18: User!
               user19: User!
               user20: User!
               user21: User!
               user22: User!
               user23: User!
             }

             type User {
               character1(name: String!): String!
               character2(name: String!): String!
               character3(name: String!): String!
               character4(name: String!): String!
               character5(name: String!): String!
               character6(name: String!): String!
               character7(name: String!): String!
               character8(name: String!): String!
               character9(name: String!): String!
               character10(name: String!): String!
               character11(name: String!): String!
               character12(name: String!): String!
               character13(name: String!): String!
               character14(name: String!): String!
               character15(name: String!): String!
               character16(name: String!): String!
               character17(name: String!): String!
               character18(name: String!): String!
               character19(name: String!): String!
               character20(name: String!): String!
               character21(name: String!): String!
               character22(name: String!): String!
               character23(name: String!): String!
             }
            """.stripMargin

        assertM(gen(schema))(
          equalTo(
            """import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Big
  object Big {

    final case class BigView[
      User1Selection,
      User2Selection,
      User3Selection,
      User4Selection,
      User5Selection,
      User6Selection,
      User7Selection,
      User8Selection,
      User9Selection,
      User10Selection,
      User11Selection,
      User12Selection,
      User13Selection,
      User14Selection,
      User15Selection,
      User16Selection,
      User17Selection,
      User18Selection,
      User19Selection,
      User20Selection,
      User21Selection,
      User22Selection,
      User23Selection
    ](
      user1: User1Selection,
      user2: User2Selection,
      user3: User3Selection,
      user4: User4Selection,
      user5: User5Selection,
      user6: User6Selection,
      user7: User7Selection,
      user8: User8Selection,
      user9: User9Selection,
      user10: User10Selection,
      user11: User11Selection,
      user12: User12Selection,
      user13: User13Selection,
      user14: User14Selection,
      user15: User15Selection,
      user16: User16Selection,
      user17: User17Selection,
      user18: User18Selection,
      user19: User19Selection,
      user20: User20Selection,
      user21: User21Selection,
      user22: User22Selection,
      user23: User23Selection
    )

    final case class BigViewSelectionArgs[
      User1Selection,
      User2Selection,
      User3Selection,
      User4Selection,
      User5Selection,
      User6Selection,
      User7Selection,
      User8Selection,
      User9Selection,
      User10Selection,
      User11Selection,
      User12Selection,
      User13Selection,
      User14Selection,
      User15Selection,
      User16Selection,
      User17Selection,
      User18Selection,
      User19Selection,
      User20Selection,
      User21Selection,
      User22Selection,
      User23Selection
    ](
      user1Selection: SelectionBuilder[User, User1Selection],
      user2Selection: SelectionBuilder[User, User2Selection],
      user3Selection: SelectionBuilder[User, User3Selection],
      user4Selection: SelectionBuilder[User, User4Selection],
      user5Selection: SelectionBuilder[User, User5Selection],
      user6Selection: SelectionBuilder[User, User6Selection],
      user7Selection: SelectionBuilder[User, User7Selection],
      user8Selection: SelectionBuilder[User, User8Selection],
      user9Selection: SelectionBuilder[User, User9Selection],
      user10Selection: SelectionBuilder[User, User10Selection],
      user11Selection: SelectionBuilder[User, User11Selection],
      user12Selection: SelectionBuilder[User, User12Selection],
      user13Selection: SelectionBuilder[User, User13Selection],
      user14Selection: SelectionBuilder[User, User14Selection],
      user15Selection: SelectionBuilder[User, User15Selection],
      user16Selection: SelectionBuilder[User, User16Selection],
      user17Selection: SelectionBuilder[User, User17Selection],
      user18Selection: SelectionBuilder[User, User18Selection],
      user19Selection: SelectionBuilder[User, User19Selection],
      user20Selection: SelectionBuilder[User, User20Selection],
      user21Selection: SelectionBuilder[User, User21Selection],
      user22Selection: SelectionBuilder[User, User22Selection],
      user23Selection: SelectionBuilder[User, User23Selection]
    )

    type ViewSelection[
      User1Selection,
      User2Selection,
      User3Selection,
      User4Selection,
      User5Selection,
      User6Selection,
      User7Selection,
      User8Selection,
      User9Selection,
      User10Selection,
      User11Selection,
      User12Selection,
      User13Selection,
      User14Selection,
      User15Selection,
      User16Selection,
      User17Selection,
      User18Selection,
      User19Selection,
      User20Selection,
      User21Selection,
      User22Selection,
      User23Selection
    ] = SelectionBuilder[Big, BigView[
      User1Selection,
      User2Selection,
      User3Selection,
      User4Selection,
      User5Selection,
      User6Selection,
      User7Selection,
      User8Selection,
      User9Selection,
      User10Selection,
      User11Selection,
      User12Selection,
      User13Selection,
      User14Selection,
      User15Selection,
      User16Selection,
      User17Selection,
      User18Selection,
      User19Selection,
      User20Selection,
      User21Selection,
      User22Selection,
      User23Selection
    ]]

    def view[
      User1Selection,
      User2Selection,
      User3Selection,
      User4Selection,
      User5Selection,
      User6Selection,
      User7Selection,
      User8Selection,
      User9Selection,
      User10Selection,
      User11Selection,
      User12Selection,
      User13Selection,
      User14Selection,
      User15Selection,
      User16Selection,
      User17Selection,
      User18Selection,
      User19Selection,
      User20Selection,
      User21Selection,
      User22Selection,
      User23Selection
    ](
      selectionArgs: BigViewSelectionArgs[
        User1Selection,
        User2Selection,
        User3Selection,
        User4Selection,
        User5Selection,
        User6Selection,
        User7Selection,
        User8Selection,
        User9Selection,
        User10Selection,
        User11Selection,
        User12Selection,
        User13Selection,
        User14Selection,
        User15Selection,
        User16Selection,
        User17Selection,
        User18Selection,
        User19Selection,
        User20Selection,
        User21Selection,
        User22Selection,
        User23Selection
      ]
    ): ViewSelection[
      User1Selection,
      User2Selection,
      User3Selection,
      User4Selection,
      User5Selection,
      User6Selection,
      User7Selection,
      User8Selection,
      User9Selection,
      User10Selection,
      User11Selection,
      User12Selection,
      User13Selection,
      User14Selection,
      User15Selection,
      User16Selection,
      User17Selection,
      User18Selection,
      User19Selection,
      User20Selection,
      User21Selection,
      User22Selection,
      User23Selection
    ] = (user1(selectionArgs.user1Selection) ~ user2(selectionArgs.user2Selection) ~ user3(
      selectionArgs.user3Selection
    ) ~ user4(selectionArgs.user4Selection) ~ user5(selectionArgs.user5Selection) ~ user6(
      selectionArgs.user6Selection
    ) ~ user7(selectionArgs.user7Selection) ~ user8(selectionArgs.user8Selection) ~ user9(
      selectionArgs.user9Selection
    ) ~ user10(selectionArgs.user10Selection) ~ user11(selectionArgs.user11Selection) ~ user12(
      selectionArgs.user12Selection
    ) ~ user13(selectionArgs.user13Selection) ~ user14(selectionArgs.user14Selection) ~ user15(
      selectionArgs.user15Selection
    ) ~ user16(selectionArgs.user16Selection) ~ user17(selectionArgs.user17Selection) ~ user18(
      selectionArgs.user18Selection
    ) ~ user19(selectionArgs.user19Selection) ~ user20(selectionArgs.user20Selection) ~ user21(
      selectionArgs.user21Selection
    ) ~ user22(selectionArgs.user22Selection) ~ user23(selectionArgs.user23Selection)).map {
      case (
            (
              (
                (
                  (
                    (
                      (
                        (
                          (
                            (
                              (
                                (
                                  (
                                    (
                                      ((((((((user1, user2), user3), user4), user5), user6), user7), user8), user9),
                                      user10
                                    ),
                                    user11
                                  ),
                                  user12
                                ),
                                user13
                              ),
                              user14
                            ),
                            user15
                          ),
                          user16
                        ),
                        user17
                      ),
                      user18
                    ),
                    user19
                  ),
                  user20
                ),
                user21
              ),
              user22
            ),
            user23
          ) =>
        BigView(
          user1,
          user2,
          user3,
          user4,
          user5,
          user6,
          user7,
          user8,
          user9,
          user10,
          user11,
          user12,
          user13,
          user14,
          user15,
          user16,
          user17,
          user18,
          user19,
          user20,
          user21,
          user22,
          user23
        )
    }

    def user1[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user1", Obj(innerSelection))
    def user2[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user2", Obj(innerSelection))
    def user3[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user3", Obj(innerSelection))
    def user4[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user4", Obj(innerSelection))
    def user5[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user5", Obj(innerSelection))
    def user6[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user6", Obj(innerSelection))
    def user7[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user7", Obj(innerSelection))
    def user8[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user8", Obj(innerSelection))
    def user9[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A]  =
      _root_.caliban.client.SelectionBuilder.Field("user9", Obj(innerSelection))
    def user10[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user10", Obj(innerSelection))
    def user11[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user11", Obj(innerSelection))
    def user12[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user12", Obj(innerSelection))
    def user13[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user13", Obj(innerSelection))
    def user14[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user14", Obj(innerSelection))
    def user15[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user15", Obj(innerSelection))
    def user16[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user16", Obj(innerSelection))
    def user17[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user17", Obj(innerSelection))
    def user18[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user18", Obj(innerSelection))
    def user19[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user19", Obj(innerSelection))
    def user20[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user20", Obj(innerSelection))
    def user21[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user21", Obj(innerSelection))
    def user22[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user22", Obj(innerSelection))
    def user23[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Big, A] =
      _root_.caliban.client.SelectionBuilder.Field("user23", Obj(innerSelection))
  }

  type User
  object User {

    final case class UserView(
      character1: String,
      character2: String,
      character3: String,
      character4: String,
      character5: String,
      character6: String,
      character7: String,
      character8: String,
      character9: String,
      character10: String,
      character11: String,
      character12: String,
      character13: String,
      character14: String,
      character15: String,
      character16: String,
      character17: String,
      character18: String,
      character19: String,
      character20: String,
      character21: String,
      character22: String,
      character23: String
    )

    final case class UserViewArgs(
      character1Name: String,
      character2Name: String,
      character3Name: String,
      character4Name: String,
      character5Name: String,
      character6Name: String,
      character7Name: String,
      character8Name: String,
      character9Name: String,
      character10Name: String,
      character11Name: String,
      character12Name: String,
      character13Name: String,
      character14Name: String,
      character15Name: String,
      character16Name: String,
      character17Name: String,
      character18Name: String,
      character19Name: String,
      character20Name: String,
      character21Name: String,
      character22Name: String,
      character23Name: String
    )

    type ViewSelection = SelectionBuilder[User, UserView]

    def view(args: UserViewArgs): ViewSelection =
      (character1(args.character1Name) ~ character2(args.character2Name) ~ character3(args.character3Name) ~ character4(
        args.character4Name
      ) ~ character5(args.character5Name) ~ character6(args.character6Name) ~ character7(
        args.character7Name
      ) ~ character8(args.character8Name) ~ character9(args.character9Name) ~ character10(
        args.character10Name
      ) ~ character11(args.character11Name) ~ character12(args.character12Name) ~ character13(
        args.character13Name
      ) ~ character14(args.character14Name) ~ character15(args.character15Name) ~ character16(
        args.character16Name
      ) ~ character17(args.character17Name) ~ character18(args.character18Name) ~ character19(
        args.character19Name
      ) ~ character20(args.character20Name) ~ character21(args.character21Name) ~ character22(
        args.character22Name
      ) ~ character23(args.character23Name)).map {
        case (
              (
                (
                  (
                    (
                      (
                        (
                          (
                            (
                              (
                                (
                                  (
                                    (
                                      (
                                        (
                                          (
                                            (
                                              (
                                                ((((character1, character2), character3), character4), character5),
                                                character6
                                              ),
                                              character7
                                            ),
                                            character8
                                          ),
                                          character9
                                        ),
                                        character10
                                      ),
                                      character11
                                    ),
                                    character12
                                  ),
                                  character13
                                ),
                                character14
                              ),
                              character15
                            ),
                            character16
                          ),
                          character17
                        ),
                        character18
                      ),
                      character19
                    ),
                    character20
                  ),
                  character21
                ),
                character22
              ),
              character23
            ) =>
          UserView(
            character1,
            character2,
            character3,
            character4,
            character5,
            character6,
            character7,
            character8,
            character9,
            character10,
            character11,
            character12,
            character13,
            character14,
            character15,
            character16,
            character17,
            character18,
            character19,
            character20,
            character21,
            character22,
            character23
          )
      }

    def character1(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character1", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character2(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character2", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character3(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character3", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character4(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character4", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character5(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character5", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character6(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character6", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character7(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character7", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character8(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character8", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character9(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String]  =
      _root_.caliban.client.SelectionBuilder
        .Field("character9", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character10(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character10", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character11(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character11", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character12(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character12", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character13(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character13", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character14(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character14", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character15(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character15", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character16(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character16", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character17(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character17", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character18(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character18", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character19(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character19", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character20(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character20", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character21(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character21", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character22(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character22", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
    def character23(name: String)(implicit encoder0: ArgEncoder[String]): SelectionBuilder[User, String] =
      _root_.caliban.client.SelectionBuilder
        .Field("character23", Scalar(), arguments = List(Argument("name", name, "String!")(encoder0)))
  }

}
"""
          )
        )
      }
    ) @@ TestAspect.sequential
}
