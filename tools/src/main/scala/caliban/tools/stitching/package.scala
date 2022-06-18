package caliban.tools

import caliban.{ CalibanError, ResponseValue }

import sttp.client3._

package object stitching {
  type HttpRequest = RequestT[Identity, Either[CalibanError.ExecutionError, ResponseValue], Any]
}
