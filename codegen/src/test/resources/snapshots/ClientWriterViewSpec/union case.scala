import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Character
  object Character {

    final case class CharacterView[RoleSelection](
      name: String,
      nicknames: List[String],
      role: scala.Option[RoleSelection]
    )

    type ViewSelection[RoleSelection] = SelectionBuilder[Character, CharacterView[RoleSelection]]

    def view[RoleSelection](
      roleSelectionOnCaptain: SelectionBuilder[Captain, RoleSelection],
      roleSelectionOnPilot: SelectionBuilder[Pilot, RoleSelection]
    ): ViewSelection[RoleSelection] = (name ~ nicknames ~ role(roleSelectionOnCaptain, roleSelectionOnPilot)).map {
      case (name, nicknames, role) => CharacterView(name, nicknames, role)
    }

    def name: SelectionBuilder[Character, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
    def role[A](
      onCaptain: SelectionBuilder[Captain, A],
      onPilot: SelectionBuilder[Pilot, A]
    ): SelectionBuilder[Character, scala.Option[A]] = _root_.caliban.client.SelectionBuilder
      .Field("role", OptionOf(ChoiceOf(Map("Captain" -> Obj(onCaptain), "Pilot" -> Obj(onPilot)))))
    def roleOption[A](
      onCaptain: scala.Option[SelectionBuilder[Captain, A]] = None,
      onPilot: scala.Option[SelectionBuilder[Pilot, A]] = None
    ): SelectionBuilder[Character, scala.Option[scala.Option[A]]] = _root_.caliban.client.SelectionBuilder.Field(
      "role",
      OptionOf(
        ChoiceOf(
          Map(
            "Captain" -> onCaptain.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a))),
            "Pilot"   -> onPilot.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a)))
          )
        )
      )
    )
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
