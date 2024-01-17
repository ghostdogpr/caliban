package caliban.client

import caliban.client.CalibanClientError.CommunicationError
import caliban.client.Operations.{ IsOperation, RootSubscription }
import caliban.client.__Value.__ObjectValue
import caliban.client.ws.{ GraphQLWSRequest, GraphQLWSResponse }
import com.github.plokhotnyuk.jsoniter_scala.core.{ writeToString, ReaderConfig }
import com.raquo.airstream.core.EventStream
import io.laminext.fetch.jsoniter._
import io.laminext.websocket.jsoniter._
import io.laminext.websocket.{ WebSocket, WebSocketBuilder, WebSocketReceiveBuilder }

import java.util.UUID
import scala.concurrent.ExecutionContext

package object laminext {

  implicit class WebSocketReceiveBuilderOps(self: WebSocketReceiveBuilder) {
    def graphql: WebSocketBuilder[GraphQLWSResponse, GraphQLWSRequest] =
      self.json[GraphQLWSResponse, GraphQLWSRequest]
  }

  implicit class WebSocketOps(self: WebSocket[GraphQLWSResponse, GraphQLWSRequest]) {
    def init(): Unit =
      self.sendOne(GraphQLWSRequest("connection_init", None, None))
  }

  implicit class SelectionBuilderOps[Origin, A](self: SelectionBuilder[Origin, A]) {

    def toEventStream(
      uri: String,
      useVariables: Boolean = false,
      queryName: Option[String] = None,
      dropNullInputValues: Boolean = false,
      middleware: FetchEventStreamBuilder => FetchEventStreamBuilder = identity,
      maxBufSize: Int = ReaderConfig.maxBufSize,
      maxCharBufSize: Int = ReaderConfig.maxCharBufSize
    )(implicit ev: IsOperation[Origin], ec: ExecutionContext): EventStream[Either[CalibanClientError, A]] =
      toEventStreamWith(uri, useVariables, queryName, dropNullInputValues, middleware, maxBufSize, maxCharBufSize)(
        (res, _, _) => res
      )

    def toEventStreamWith[B](
      uri: String,
      useVariables: Boolean = false,
      queryName: Option[String] = None,
      dropNullInputValues: Boolean = false,
      middleware: FetchEventStreamBuilder => FetchEventStreamBuilder = identity,
      maxBufSize: Int = ReaderConfig.maxBufSize,
      maxCharBufSize: Int = ReaderConfig.maxCharBufSize
    )(
      mapResponse: (A, List[GraphQLResponseError], Option[__ObjectValue]) => B
    )(implicit ev: IsOperation[Origin], ec: ExecutionContext): EventStream[Either[CalibanClientError, B]] =
      middleware(Fetch.post(uri))
        .body(self.toGraphQL(useVariables, queryName, dropNullInputValues))
        .text
        .map(response =>
          if (response.ok)
            self.decode(response.data, maxBufSize, maxCharBufSize).map { case (result, errors, extensions) =>
              mapResponse(result, errors, extensions)
            }
          else throw CommunicationError(s"Received status ${response.status}. Body: ${response.data}")
        )
        .recover {
          case err: CalibanClientError => Some(Left(err))
          case other                   => Some(Left(CommunicationError("", Some(other))))
        }

    def toSubscription(
      ws: WebSocket[GraphQLWSResponse, GraphQLWSRequest],
      useVariables: Boolean = false,
      queryName: Option[String] = None,
      maxBufSize: Int = ReaderConfig.maxBufSize,
      maxCharBufSize: Int = ReaderConfig.maxCharBufSize
    )(implicit ev1: IsOperation[Origin], ev2: Origin <:< RootSubscription): Subscription[A] = {
      val subscription = toSubscriptionWith(ws, useVariables, queryName, maxBufSize, maxCharBufSize)((res, _, _) => res)
      new Subscription[A] {
        def received: EventStream[Either[CalibanClientError, A]] = subscription.received
        def unsubscribe(): Unit                                  = subscription.unsubscribe()
      }
    }

    def toSubscriptionWith[B](
      ws: WebSocket[GraphQLWSResponse, GraphQLWSRequest],
      useVariables: Boolean = false,
      queryName: Option[String] = None,
      maxBufSize: Int = ReaderConfig.maxBufSize,
      maxCharBufSize: Int = ReaderConfig.maxCharBufSize
    )(
      mapResponse: (A, List[GraphQLResponseError], Option[__ObjectValue]) => B
    )(implicit ev1: IsOperation[Origin], ev2: Origin <:< RootSubscription): Subscription[B] = {
      val id = UUID.randomUUID().toString
      ws.sendOne(GraphQLWSRequest("start", Some(id), Some(self.toGraphQL(useVariables, queryName))))
      new Subscription[B] {
        def received: EventStream[Either[CalibanClientError, B]] =
          ws.received.collect { case GraphQLWSResponse("data", Some(`id`), Some(payload)) =>
            self.decode(writeToString(payload), maxBufSize, maxCharBufSize).map { case (result, errors, extensions) =>
              mapResponse(result, errors, extensions)
            }
          }

        def unsubscribe(): Unit =
          ws.sendOne(GraphQLWSRequest("stop", Some(id), None))
      }
    }
  }
}
