import caliban.client.FieldBuilder._
import caliban.client._

object Client {

  type Node
  object Node {

    final case class NodeView(id: String)

    type ViewSelection = SelectionBuilder[Node, NodeView]

    def view: ViewSelection = id.map(id => NodeView(id))

    def id: SelectionBuilder[Node, String] = _root_.caliban.client.SelectionBuilder.Field("id", Scalar())
  }

  type NodeA
  object NodeA {

    final case class NodeAView(id: String, a: scala.Option[String])

    type ViewSelection = SelectionBuilder[NodeA, NodeAView]

    def view: ViewSelection = (id ~ a).map { case (id, a) => NodeAView(id, a) }

    def id: SelectionBuilder[NodeA, String]              = _root_.caliban.client.SelectionBuilder.Field("id", Scalar())
    def a: SelectionBuilder[NodeA, scala.Option[String]] =
      _root_.caliban.client.SelectionBuilder.Field("a", OptionOf(Scalar()))
  }

  type NodeB
  object NodeB {

    final case class NodeBView(id: String, b: scala.Option[Int])

    type ViewSelection = SelectionBuilder[NodeB, NodeBView]

    def view: ViewSelection = (id ~ b).map { case (id, b) => NodeBView(id, b) }

    def id: SelectionBuilder[NodeB, String]           = _root_.caliban.client.SelectionBuilder.Field("id", Scalar())
    def b: SelectionBuilder[NodeB, scala.Option[Int]] =
      _root_.caliban.client.SelectionBuilder.Field("b", OptionOf(Scalar()))
  }

  type Queries = _root_.caliban.client.Operations.RootQuery
  object Queries {
    def node[A](id: String)(onNodeA: SelectionBuilder[NodeA, A], onNodeB: SelectionBuilder[NodeB, A])(implicit
      encoder0: ArgEncoder[String]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "node",
        OptionOf(ChoiceOf(Map("NodeA" -> Obj(onNodeA), "NodeB" -> Obj(onNodeB)))),
        arguments = List(Argument("id", id, "ID!")(encoder0))
      )
    def nodeOption[A](id: String)(
      onNodeA: scala.Option[SelectionBuilder[NodeA, A]] = None,
      onNodeB: scala.Option[SelectionBuilder[NodeB, A]] = None
    )(implicit
      encoder0: ArgEncoder[String]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootQuery, scala.Option[scala.Option[A]]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "node",
        OptionOf(
          ChoiceOf(
            Map(
              "NodeA" -> onNodeA.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a))),
              "NodeB" -> onNodeB.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a)))
            )
          )
        ),
        arguments = List(Argument("id", id, "ID!")(encoder0))
      )
  }

  type Mutations = _root_.caliban.client.Operations.RootMutation
  object Mutations {
    def updateNode[A](
      id: String,
      name: scala.Option[String] = None
    )(onNodeA: SelectionBuilder[NodeA, A], onNodeB: SelectionBuilder[NodeB, A])(implicit
      encoder0: ArgEncoder[String],
      encoder1: ArgEncoder[scala.Option[String]]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootMutation, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "updateNode",
        OptionOf(ChoiceOf(Map("NodeA" -> Obj(onNodeA), "NodeB" -> Obj(onNodeB)))),
        arguments = List(Argument("id", id, "ID!")(encoder0), Argument("name", name, "String")(encoder1))
      )
    def updateNodeOption[A](id: String, name: scala.Option[String] = None)(
      onNodeA: scala.Option[SelectionBuilder[NodeA, A]] = None,
      onNodeB: scala.Option[SelectionBuilder[NodeB, A]] = None
    )(implicit
      encoder0: ArgEncoder[String],
      encoder1: ArgEncoder[scala.Option[String]]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootMutation, scala.Option[scala.Option[A]]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "updateNode",
        OptionOf(
          ChoiceOf(
            Map(
              "NodeA" -> onNodeA.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a))),
              "NodeB" -> onNodeB.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a)))
            )
          )
        ),
        arguments = List(Argument("id", id, "ID!")(encoder0), Argument("name", name, "String")(encoder1))
      )
  }

  type Subscriptions = _root_.caliban.client.Operations.RootSubscription
  object Subscriptions {
    def node[A](id: String)(onNodeA: SelectionBuilder[NodeA, A], onNodeB: SelectionBuilder[NodeB, A])(implicit
      encoder0: ArgEncoder[String]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootSubscription, scala.Option[A]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "node",
        OptionOf(ChoiceOf(Map("NodeA" -> Obj(onNodeA), "NodeB" -> Obj(onNodeB)))),
        arguments = List(Argument("id", id, "ID!")(encoder0))
      )
    def nodeOption[A](id: String)(
      onNodeA: scala.Option[SelectionBuilder[NodeA, A]] = None,
      onNodeB: scala.Option[SelectionBuilder[NodeB, A]] = None
    )(implicit
      encoder0: ArgEncoder[String]
    ): SelectionBuilder[_root_.caliban.client.Operations.RootSubscription, scala.Option[scala.Option[A]]] =
      _root_.caliban.client.SelectionBuilder.Field(
        "node",
        OptionOf(
          ChoiceOf(
            Map(
              "NodeA" -> onNodeA.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a))),
              "NodeB" -> onNodeB.fold[FieldBuilder[scala.Option[A]]](NullField)(a => OptionOf(Obj(a)))
            )
          )
        ),
        arguments = List(Argument("id", id, "ID!")(encoder0))
      )
  }

}
