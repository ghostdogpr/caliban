package caliban.tools

import caliban.parsing.Parser
import zio.Task
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test._

object ClientWriterViewSpec extends DefaultRunnableSpec {

  val gen: String => Task[String] = (schema: String) =>
    Parser
      .parseQuery(schema)
      .flatMap(doc => Formatter.format(ClientWriter.write(doc, "Client", None, genView = true), None))

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
import caliban.client.SelectionBuilder._
import caliban.client._

object Client {

  type Character
  object Character {

    final case class CharacterView(name: String, age: Int, nicknames: List[String])

    type ViewSelection = SelectionBuilder[Character, CharacterView]

    def view: ViewSelection = (name ~ age ~ nicknames).map {
      case ((name, age), nicknames) => CharacterView(name, age, nicknames)
    }

    def name: SelectionBuilder[Character, String]            = Field("name", Scalar())
    def age: SelectionBuilder[Character, Int]                = Field("age", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] = Field("nicknames", ListOf(Scalar()))
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
import caliban.client.SelectionBuilder._
import caliban.client._

object Client {

  type Q
  object Q {

    final case class QView[UsersSelection](users: List[UsersSelection])

    type ViewSelection[UsersSelection] = SelectionBuilder[Q, QView[UsersSelection]]

    def view[UsersSelection](usersSelection: SelectionBuilder[User, UsersSelection]): ViewSelection[UsersSelection] =
      users(usersSelection).map(users => QView(users))

    def users[A](innerSelection: SelectionBuilder[User, A]): SelectionBuilder[Q, List[A]] =
      Field("users", ListOf(Obj(innerSelection)))
  }

  type Character
  object Character {

    final case class CharacterView(name: String, age: Int, nicknames: List[String])

    type ViewSelection = SelectionBuilder[Character, CharacterView]

    def view(nicknamesArg: Option[Int] = None): ViewSelection = (name ~ age ~ nicknames(nicknamesArg)).map {
      case ((name, age), nicknames) => CharacterView(name, age, nicknames)
    }

    def name: SelectionBuilder[Character, String] = Field("name", Scalar())
    def age: SelectionBuilder[Character, Int]     = Field("age", Scalar())
    def nicknames(arg: Option[Int] = None): SelectionBuilder[Character, List[String]] =
      Field("nicknames", ListOf(Scalar()), arguments = List(Argument("arg", arg)))
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
      Field("characters", ListOf(Obj(innerSelection)), arguments = List(Argument("name", name)))
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
import caliban.client.SelectionBuilder._
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

    def name: SelectionBuilder[Character, String] = Field("name", Scalar())
    def age: SelectionBuilder[Character, Int]     = Field("age", Scalar())
    def friends[A](
      filter: Option[String] = None
    )(innerSelection: SelectionBuilder[Character, A]): SelectionBuilder[Character, List[A]] =
      Field("friends", ListOf(Obj(innerSelection)), arguments = List(Argument("filter", filter)))
  }

}
"""
          )
        )
      }
    ) @@ TestAspect.sequential
}
