package poc.caliban.client.generated.potatoes

import caliban.client.FieldBuilder._
import caliban.client._

object Subscription {
  def allPotatoes[A](
      innerSelection: SelectionBuilder[Potato, A]
  ): SelectionBuilder[
    _root_.caliban.client.Operations.RootSubscription,
    scala.Option[A]
  ] = _root_.caliban.client.SelectionBuilder
    .Field("allPotatoes", OptionOf(Obj(innerSelection)))
}

