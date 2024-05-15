import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type `Captain_`
  object `Captain_` {
    def shipName: SelectionBuilder[`Captain_`, String] =
      _root_.caliban.client.SelectionBuilder.Field("shipName", Scalar())
  }

  type Pilot
  object Pilot {
    def shipName: SelectionBuilder[Pilot, String] = _root_.caliban.client.SelectionBuilder.Field("shipName", Scalar())
  }

  type Character
  object Character {
    def role[A](
      `onCaptain_`: SelectionBuilder[`Captain_`, A],
      onPilot: SelectionBuilder[Pilot, A]
    ): SelectionBuilder[Character, scala.Option[A]] = _root_.caliban.client.SelectionBuilder
      .Field("role", OptionOf(ChoiceOf(Map("Captain_" -> Obj(`onCaptain_`), "Pilot" -> Obj(onPilot)))))
    def roleOption[A](
      `onCaptain_`: scala.Option[SelectionBuilder[`Captain_`, A]] = None,
      onPilot: scala.Option[SelectionBuilder[Pilot, A]] = None
    ): SelectionBuilder[Character, scala.Option[scala.Option[A]]] = _root_.caliban.client.SelectionBuilder.Field(
      "role",
      OptionOf(
        ChoiceOf(
          Map(
            "Captain_" -> `onCaptain_`.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a))),
            "Pilot"    -> onPilot.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a)))
          )
        )
      )
    )
  }

}
