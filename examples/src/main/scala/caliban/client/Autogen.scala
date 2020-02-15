package caliban.client

import caliban.client.CalibanClientError.DecodingError
import caliban.client.FieldBuilder._
import caliban.client.SelectionBuilder._
import caliban.client.Operations._
import caliban.client.Value._
import Autogen.Role._

object Autogen {

  sealed trait Origin extends Product with Serializable
  object Origin {
    case object BELT  extends Origin
    case object EARTH extends Origin
    case object MARS  extends Origin

    implicit val decoder: ScalarDecoder[Origin] = {
      case StringValue("BELT")  => Right(Origin.BELT)
      case StringValue("EARTH") => Right(Origin.EARTH)
      case StringValue("MARS")  => Right(Origin.MARS)
      case other                => Left(DecodingError(s"Can't build Origin from input $other"))
    }
    implicit val encoder: ArgEncoder[Origin] = new ArgEncoder[Origin] {
      override def encode(value: Origin): Value = value match {
        case BELT  => StringValue("BELT")
        case EARTH => StringValue("EARTH")
        case MARS  => StringValue("MARS")
      }
      override def typeName: String = "Origin"
    }
  }

  object Role {
    type Captain
    object Captain {
      def shipName: SelectionBuilder[Captain, String] = Field("shipName", Scalar())
    }
    type Engineer
    object Engineer {
      def shipName: SelectionBuilder[Engineer, String] = Field("shipName", Scalar())
    }
    type Mechanic
    object Mechanic {
      def shipName: SelectionBuilder[Mechanic, String] = Field("shipName", Scalar())
    }
    type Pilot
    object Pilot {
      def shipName: SelectionBuilder[Pilot, String] = Field("shipName", Scalar())
    }
  }

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] = Field("nicknames", ListOf(Scalar()))
    def origin: SelectionBuilder[Character, Origin]          = Field("origin", Scalar())
    def role[A](
      onCaptain: SelectionBuilder[Captain, A],
      onEngineer: SelectionBuilder[Engineer, A],
      onMechanic: SelectionBuilder[Mechanic, A],
      onPilot: SelectionBuilder[Pilot, A]
    ): SelectionBuilder[Character, Option[A]] =
      Field(
        "role",
        OptionOf(
          Union(
            Map(
              "Captain"  -> Obj(onCaptain),
              "Engineer" -> Obj(onEngineer),
              "Mechanic" -> Obj(onMechanic),
              "Pilot"    -> Obj(onPilot)
            )
          )
        )
      )
  }

  object Queries {
    def characters[A](
      origin: Option[Origin]
    )(innerSelection: SelectionBuilder[Character, A]): SelectionBuilder[RootQuery, List[A]] =
      Field("characters", ListOf(Obj(innerSelection)), arguments = List(Argument("origin", origin)))
    def character[A](
      name: String
    )(innerSelection: SelectionBuilder[Character, A]): SelectionBuilder[RootQuery, Option[A]] =
      Field("character", OptionOf(Obj(innerSelection)), arguments = List(Argument("name", name)))
  }

  object Mutations {
    def deleteCharacter(name: String): SelectionBuilder[RootMutation, Boolean] =
      Field("deleteCharacter", Scalar(), arguments = List(Argument("name", name)))
  }

}
