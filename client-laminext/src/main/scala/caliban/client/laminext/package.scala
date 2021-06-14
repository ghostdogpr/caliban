package caliban.client

import caliban.client.CalibanClientError.CommunicationError
import caliban.client.Operations.{ IsOperation, RootSubscription }
import caliban.client.ws.{ GraphQLWSRequest, GraphQLWSResponse }
import com.raquo.airstream.core.EventStream
import io.circe.Json
import io.laminext.fetch.circe._
import io.laminext.websocket.circe._
import io.laminext.websocket.{ WebSocket, WebSocketBuilder, WebSocketReceiveBuilder }

import java.util.UUID

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

    def toEventStream[A1 >: A](
      uri: String,
      useVariables: Boolean = false,
      queryName: Option[String] = None,
      dropNullInputValues: Boolean = false,
      middleware: FetchEventStreamBuilder => FetchEventStreamBuilder = identity
    )(implicit ev: IsOperation[Origin]): EventStream[Either[CalibanClientError, A]] =
      toEventStreamWith[A1, A](uri, useVariables, queryName, dropNullInputValues, middleware)((res, _, _) => res)

    def toEventStreamWith[A1 >: A, B](
      uri: String,
      useVariables: Boolean = false,
      queryName: Option[String] = None,
      dropNullInputValues: Boolean = false,
      middleware: FetchEventStreamBuilder => FetchEventStreamBuilder = identity
    )(
      mapResponse: (A, List[GraphQLResponseError], Option[Json]) => B
    )(implicit ev: IsOperation[Origin]): EventStream[Either[CalibanClientError, B]] =
      middleware(Fetch.post(uri))
        .body(self.toGraphQL(useVariables, queryName, dropNullInputValues))
        .text
        .map(response =>
          self.decode(response.data).map { case (result, errors, extensions) =>
            mapResponse(result, errors, extensions)
          }
        )
        .recover {
          case err: CalibanClientError => Some(Left(err))
          case other                   => Some(Left(CommunicationError("", Some(other))))
        }

    def toSubscription[A1 >: A](
      ws: WebSocket[GraphQLWSResponse, GraphQLWSRequest],
      useVariables: Boolean = false,
      queryName: Option[String] = None
    )(implicit ev1: IsOperation[Origin], ev2: Origin <:< RootSubscription): Subscription[A] = {
      val subscription = toSubscriptionWith[A1, A](ws, useVariables, queryName)((res, _, _) => res)
      new Subscription[A] {
        def received: EventStream[Either[CalibanClientError, A]] = subscription.received
        def unsubscribe(): Unit                                  = subscription.unsubscribe()
      }
    }

    def toSubscriptionWith[A1 >: A, B](
      ws: WebSocket[GraphQLWSResponse, GraphQLWSRequest],
      useVariables: Boolean = false,
      queryName: Option[String] = None
    )(
      mapResponse: (A, List[GraphQLResponseError], Option[Json]) => B
    )(implicit ev1: IsOperation[Origin], ev2: Origin <:< RootSubscription): Subscription[B] = {
      val id = UUID.randomUUID().toString
      ws.sendOne(GraphQLWSRequest("start", Some(id), Some(self.toGraphQL(useVariables, queryName))))
      new Subscription[B] {
        def received: EventStream[Either[CalibanClientError, B]] =
          ws.received.collect { case GraphQLWSResponse("data", Some(`id`), Some(payload)) =>
            self.decode(payload.noSpaces).map { case (result, errors, extensions) =>
              mapResponse(result, errors, extensions)
            }
          }

        def unsubscribe(): Unit =
          ws.sendOne(GraphQLWSRequest("stop", Some(id), None))
      }
    }
  }
}
