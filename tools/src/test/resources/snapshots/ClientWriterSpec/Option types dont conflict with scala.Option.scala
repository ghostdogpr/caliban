import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Option
  object Option {
    def id: SelectionBuilder[Option, UUID]                                                                      = _root_.caliban.client.SelectionBuilder.Field("id", Scalar())
    def name: SelectionBuilder[Option, scala.Option[String]]                                                    =
      _root_.caliban.client.SelectionBuilder.Field("name", OptionOf(Scalar()))
    def character[A](innerSelection: SelectionBuilder[Character, A]): SelectionBuilder[Option, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field("character", OptionOf(Obj(innerSelection)))
  }

  type Character
  object Character {
    def name: SelectionBuilder[Character, String]            = _root_.caliban.client.SelectionBuilder.Field("name", Scalar())
    def nicknames: SelectionBuilder[Character, List[String]] =
      _root_.caliban.client.SelectionBuilder.Field("nicknames", ListOf(Scalar()))
  }

  type Query = _root_.caliban.client.Operations.RootQuery
  object Query {
    def maybeOption[A](id: UUID)(innerSelection: SelectionBuilder[Option, A])(implicit
      encoder0: ArgEncoder[UUID]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder
        .Field("maybeOption", OptionOf(Obj(innerSelection)), arguments = List(Argument("id", id, "UUID!")(encoder0)))
    def someOption[A](id: UUID)(innerSelection: SelectionBuilder[Option, A])(implicit
      encoder0: ArgEncoder[UUID]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, A] = _root_.caliban.client.SelectionBuilder
      .Field("someOption", Obj(innerSelection), arguments = List(Argument("id", id, "UUID!")(encoder0)))
    def maybeOptionList[A](id: UUID)(innerSelection: SelectionBuilder[Option, A])(implicit
      encoder0: ArgEncoder[UUID]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, scala.Option[List[A]]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "maybeOptionList",
        OptionOf(ListOf(Obj(innerSelection))),
        arguments = List(Argument("id", id, "UUID!")(encoder0))
      )
    def someOptionList[A](id: UUID)(innerSelection: SelectionBuilder[Option, A])(implicit
      encoder0: ArgEncoder[UUID]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, List[A]] = _root_.caliban.client.SelectionBuilder
      .Field("someOptionList", ListOf(Obj(innerSelection)), arguments = List(Argument("id", id, "UUID!")(encoder0)))
    def maybeMaybeOptionList[A](
      innerSelection: SelectionBuilder[Option, A]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, scala.Option[List[scala.Option[A]]]] =
      _root_.caliban.client.SelectionBuilder
        .Field("maybeMaybeOptionList", OptionOf(ListOf(OptionOf(Obj(innerSelection)))))
  }

}
