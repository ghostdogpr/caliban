package caliban.client

import caliban.Value.StringValue
import caliban.client.Autogen.Role._
import caliban.client.FieldType._
import caliban.client.Operations._
import caliban.client.SelectionSet.Field

object Autogen {

//  case class CharacterView(name: String, nickname: List[String], origin: Origin)
//
//  val character: SelectionSet[Character, CharacterView] =
//    (Character.name ~
//      Character.nicknames ~
//      Character.origin).mapN(CharacterView)
//
//  val query: SelectionSet[RootQuery, Option[CharacterView]] =
//    Queries.character("Amos Burton") {
//      character
//    }

  // Auto-generated enum
  sealed trait Origin
  object Origin {
    case object EARTH extends Origin
    case object MARS  extends Origin
    case object BELT  extends Origin

    implicit val originDecoder: ScalarDecoder[Origin] = {
      case StringValue("EARTH") => Right(Origin.EARTH)
      case StringValue("MARS")  => Right(Origin.MARS)
      case StringValue("BELT")  => Right(Origin.BELT)
      case other                => Left(s"Can't build an Origin from input $other")
    }
    implicit val originEncoder: ArgEncoder[Origin] = {
      case EARTH => "EARTH"
      case MARS  => "MARS"
      case BELT  => "BELT"
    }
  }

  // Auto-generated union
  object Role {
    type Captain
    object Captain {
      def shipName: SelectionSet[Captain, String] = Field("shipName", Scalar())
    }
    type Pilot
    object Pilot {
      def shipName: SelectionSet[Pilot, String] = Field("shipName", Scalar())
    }
    type Mechanic
    object Mechanic {
      def shipName: SelectionSet[Mechanic, String] = Field("shipName", Scalar())
    }
    type Engineer
    object Engineer {
      def shipName: SelectionSet[Engineer, String] = Field("shipName", Scalar())
    }
  }

  // Auto-generated object
  type Character
  object Character {
    def name: SelectionSet[Character, String]            = Field("name", Scalar())
    def nicknames: SelectionSet[Character, List[String]] = Field("nicknames", ListOf(Scalar()))
    def origin: SelectionSet[Character, Origin]          = Field("origin", Scalar())
    def role[A](
      onCaptain: SelectionSet[Captain, A],
      onPilot: SelectionSet[Pilot, A],
      onMechanic: SelectionSet[Mechanic, A],
      onEngineer: SelectionSet[Engineer, A]
    ): SelectionSet[Character, Option[A]] =
      Field(
        "role",
        OptionOf(
          Union(Map("Captain" -> onCaptain, "Pilot" -> onPilot, "Mechanic" -> onMechanic, "Engineer" -> onEngineer))
        )
      )
  }

  // Auto-generated query
  object Queries {
    def characters[A](
      origin: Option[Origin] = None
    )(sel: SelectionSet[Character, A]): SelectionSet[RootQuery, List[A]] =
      Field("characters", ListOf(Obj(sel)), List(Argument("origin", origin)))

    def character[A](name: String)(sel: SelectionSet[Character, A]): Field[RootQuery, Option[A]] =
      Field("character", OptionOf(Obj(sel)), List(Argument("name", name)))
  }

  // Auto-generated mutation
  object Mutations {
    def deleteCharacter(name: String): SelectionSet[RootMutation, Boolean] =
      Field("deleteCharacter", Scalar(), List(Argument("name", name)))
  }
}
