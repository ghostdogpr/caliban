package caliban.client

import caliban.client.CalibanClientError.DecodingError
import caliban.client.FieldBuilder._
import caliban.client.Operations.RootQuery
import caliban.client.SelectionBuilder.Field
import caliban.client.TestData.Role._
import caliban.client.__Value.__StringValue

object TestData {

  sealed trait Origin
  object Origin {
    case object EARTH extends Origin
    case object MARS  extends Origin
    case object BELT  extends Origin

    implicit val originDecoder: ScalarDecoder[Origin] = {
      case __StringValue("EARTH") => Right(Origin.EARTH)
      case __StringValue("MARS")  => Right(Origin.MARS)
      case __StringValue("BELT")  => Right(Origin.BELT)
      case other                  => Left(DecodingError(s"Can't build an Origin from input $other"))
    }
    implicit val originEncoder: ArgEncoder[Origin]    = new ArgEncoder[Origin] {
      override def encode(value: Origin): __Value = value match {
        case EARTH => __StringValue("EARTH")
        case MARS  => __StringValue("MARS")
        case BELT  => __StringValue("BELT")
      }
      override def typeName: String               = "Origin"
    }
  }

  object Role {
    type Captain
    object Captain  {
      def shipName: SelectionBuilder[Captain, String] = Field("shipName", Scalar())
    }
    type Pilot
    object Pilot    {
      def shipName: SelectionBuilder[Pilot, String] = Field("shipName", Scalar())
    }
    type Mechanic
    object Mechanic {
      def shipName: SelectionBuilder[Mechanic, String] = Field("shipName", Scalar())
    }
    type Engineer
    object Engineer {
      def shipName: SelectionBuilder[Engineer, String] = Field("shipName", Scalar())
    }
  }

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] = Field("nicknames", ListOf(Scalar()))
    def origin: SelectionBuilder[Character, Origin]          = Field("origin", Scalar())
    def role[A](
      onCaptain: SelectionBuilder[Captain, A],
      onPilot: SelectionBuilder[Pilot, A],
      onMechanic: SelectionBuilder[Mechanic, A],
      onEngineer: SelectionBuilder[Engineer, A]
    ): SelectionBuilder[Character, Option[A]]                =
      Field(
        "role",
        OptionOf(
          ChoiceOf(
            Map(
              "Captain"  -> Obj(onCaptain),
              "Pilot"    -> Obj(onPilot),
              "Mechanic" -> Obj(onMechanic),
              "Engineer" -> Obj(onEngineer)
            )
          )
        )
      )
  }

  // Auto-generated query
  object Queries {
    def characters[A](
      origin: Option[Origin] = None
    )(sel: SelectionBuilder[Character, A]): SelectionBuilder[RootQuery, List[A]] =
      Field("characters", ListOf(Obj(sel)), arguments = List(Argument("origin", origin)))

    def character[A](name: String)(sel: SelectionBuilder[Character, A]): Field[RootQuery, Option[A]] =
      Field("character", OptionOf(Obj(sel)), arguments = List(Argument("name", name)))
  }

}
