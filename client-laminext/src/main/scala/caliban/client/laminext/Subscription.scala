package caliban.client.laminext

import caliban.client.CalibanClientError
import com.raquo.airstream.core.EventStream

trait Subscription[A] {
  def received: EventStream[Either[CalibanClientError, A]]
  def unsubscribe(): Unit
}
