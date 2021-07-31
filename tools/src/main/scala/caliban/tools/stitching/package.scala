package caliban.tools

import caliban.{ CalibanError, ResponseValue }

import sttp.client3._
import sttp.client3.circe._
import sttp.client3.asynchttpclient.zio._

package object stitching {
  type HttpRequest = RequestT[Identity, Either[CalibanError.ExecutionError, ResponseValue], Any]
}
