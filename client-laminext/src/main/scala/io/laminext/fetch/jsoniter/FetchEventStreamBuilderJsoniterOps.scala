package io.laminext.fetch
package jsoniter

import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromString, JsonValueCodec }
import com.raquo.laminar.api.L._
import org.scalajs.dom.Response

import scala.concurrent.{ ExecutionContext, Future }
import scala.scalajs.js.Thenable.Implicits._
import scala.util.control.NonFatal

class FetchEventStreamBuilderJsoniterOps(underlying: FetchEventStreamBuilder) {

  private def decodeResponse[A](
    response: Response
  )(implicit codec: JsonValueCodec[A], ec: ExecutionContext): Future[A] =
    response.text().flatMap { text =>
      try Future.successful(readFromString[A](text)) match {
        case NonFatal(e) => Future.failed(ResponseError(e, response))
      }
    }

  private def acceptJson(b: FetchEventStreamBuilder): FetchEventStreamBuilder =
    b.updateHeaders(_.updated("accept", "application/json"))

  def decode[A](implicit decoder: JsonValueCodec[A], ec: ExecutionContext): EventStream[FetchResponse[A]] =
    acceptJson(underlying).build(decodeResponse[A](_))

  def decodeEither[NonOkay, Okay](implicit
    decodeNonOkay: JsonValueCodec[NonOkay],
    decodeOkay: JsonValueCodec[Okay],
    ec: ExecutionContext
  ): EventStream[FetchResponse[Either[NonOkay, Okay]]] =
    acceptJson(underlying).build { response =>
      if (response.ok) {
        decodeResponse[Okay](response).map(Right(_))
      } else {
        decodeResponse[NonOkay](response).map(Left(_))
      }
    }

  def decodeOkay[Okay](implicit
    decodeOkay: JsonValueCodec[Okay],
    ec: ExecutionContext
  ): EventStream[FetchResponse[Okay]] =
    acceptJson(underlying).build { response =>
      if (response.ok) {
        decodeResponse(response)
      } else {
        Future.failed(new NonOkayResponse(response))
      }
    }

}
