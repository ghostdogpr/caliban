package caliban.tools

import caliban.parsing.Parser
import caliban.tools.implicits.ScalarMappings
import zio.Task
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object ClientWriterViewSpec extends DefaultRunnableSpec {

  val gen: String => Task[String] = (schema: String) =>
    Parser
      .parseQuery(schema)
      .flatMap(doc => Formatter.format(ClientWriter.write(doc, genView = true)(ScalarMappings(None)), None))

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

    def name: SelectionBuilder[Character, String]                                     = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def age: SelectionBuilder[Character, Int]                                         = _root_.caliban.client.SelectionBuilder.Field("age", Scalar())
    def nicknames(arg: Option[Int] = None): SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder
        .Field("nicknames", ListOf(Scalar()), arguments = List(Argument("arg", arg, "Int")))
  }

  type User
  object User {

    final case class UserView[CharactersSelection](characters: List[CharactersSelection])

    type ViewSelection[CharactersSelection] = SelectionBuilder[User, UserView[CharactersSelection]]

    def view[CharactersSelection](
      charactersName: String
    )(charactersSelection: SelectionBuilder[Character, CharactersSelection]): ViewSelection[CharactersSelection] =
      characters(charactersName)(charactersSelection).map(characters => UserView(characters))

    def characters[A](name: String)(innerSelection: SelectionBuilder[Character, A]): SelectionBuilder[User, List[A]] =
      _root_.caliban.client.SelectionBuilder
        .Field("characters", ListOf(Obj(innerSelection)), arguments = List(Argument("name", name, "String!")))
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
    def friends[A](filter: Option[String] = None)(
      innerSelection: SelectionBuilder[Character, A]
    ): SelectionBuilder[Character, List[A]]       = _root_.caliban.client.SelectionBuilder
      .Field("friends", ListOf(Obj(innerSelection)), arguments = List(Argument("filter", filter, "String")))
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
    ): SelectionBuilder[ProjectMemberConnection, Option[List[Option[A]]]]                                        =
      _root_.caliban.client.SelectionBuilder.Field("edges", OptionOf(ListOf(OptionOf(Obj(innerSelection)))))
    def nodes[A](
      innerSelection: SelectionBuilder[ProjectMember, A]
    ): SelectionBuilder[ProjectMemberConnection, Option[List[Option[A]]]]                                        =
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
    ): SelectionBuilder[Character, Option[A]]                = _root_.caliban.client.SelectionBuilder
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
      }
    ) @@ TestAspect.sequential
}
