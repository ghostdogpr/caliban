import caliban.client.FieldBuilder._
import caliban.client._

import java.util.UUID
import a.b._

object Client {

  type Order
  object Order {
    def date: SelectionBuilder[Order, java.time.OffsetDateTime] =
      _root_.caliban.client.SelectionBuilder.Field("date", Scalar())
  }

}
