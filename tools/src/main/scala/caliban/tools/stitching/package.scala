package caliban.tools

import caliban.{ CalibanError, ResponseValue }

import sttp.client4._

package object stitching {
  type HttpRequest = Request[Either[CalibanError.ExecutionError, ResponseValue]]
}
