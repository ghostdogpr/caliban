package caliban.client

import caliban.client.CalibanClientError.{ CommunicationError, DecodingError, ServerError }
import caliban.client.Operations.{ IsOperation, RootSubscription }
import caliban.client.__Value.__ObjectValue
import com.raquo.airstream.core.EventStream
import io.circe.{ parser, Json }
import io.laminext.fetch.circe._
import io.laminext.websocket.circe._
import io.laminext.websocket.{ WebSocket, WebSocketBuilder, WebSocketReceiveBuilder }

import java.util.UUID

package object laminext {

  implicit class WebSocketReceiveBuilderOps(self: WebSocketReceiveBuilder) {
    def graphql: WebSocketBuilder[GraphQLWSResponse, GraphQLWSRequest] = self
      .json[GraphQLWSResponse, GraphQLWSRequest]
  }

  implicit class WebSocketOps(self: WebSocket[GraphQLWSResponse, GraphQLWSRequest]) {
    def init(): Unit = self.sendOne(GraphQLWSRequest("connection_init", None, None))
  }

  implicit class SelectionBuilderOps[Origin, A](self: SelectionBuilder[Origin, A]) {

    def toEventStream[A1 >: A](
      uri: String,
      useVariables: Boolean = false,
      queryName: Option[String] = None,
      middleware: FetchEventStreamBuilder => FetchEventStreamBuilder = identity
    )(implicit ev: IsOperation[Origin]): EventStream[Either[CalibanClientError, A]] =
      toEventStreamWithExtensions[A1](uri, useVariables, queryName, middleware).map(_.map(_._1))

    def toEventStreamWithExtensions[A1 >: A](
      uri: String,
      useVariables: Boolean = false,
      queryName: Option[String] = None,
      middleware: FetchEventStreamBuilder => FetchEventStreamBuilder = identity
    )(implicit ev: IsOperation[Origin]): EventStream[Either[CalibanClientError, (A, Option[Json])]] =
      middleware(Fetch.post(uri))
        .body(self.toGraphQL(useVariables, queryName))
        .text
        .map(response => decode(response.data))
        .recover {
          case err: CalibanClientError => Some(Left(err))
          case other                   => Some(Left(CommunicationError("", Some(other))))
        }

    def toSubscription[A1 >: A](
      ws: WebSocket[GraphQLWSResponse, GraphQLWSRequest],
      useVariables: Boolean = false,
      queryName: Option[String] = None
    )(implicit ev1: IsOperation[Origin], ev2: Origin <:< RootSubscription): Subscription[A] = {
      val subscription = toSubscriptionWithExtensions[A1](ws, useVariables, queryName)
      new Subscription[A] {
        def received: EventStream[Either[CalibanClientError, A]] = subscription.received.map(_.map(_._1))
        def unsubscribe(): Unit                                  = subscription.unsubscribe()
      }
    }

    def toSubscriptionWithExtensions[A1 >: A](
      ws: WebSocket[GraphQLWSResponse, GraphQLWSRequest],
      useVariables: Boolean = false,
      queryName: Option[String] = None
    )(implicit ev1: IsOperation[Origin], ev2: Origin <:< RootSubscription): Subscription[(A, Option[Json])] = {
      val id = UUID.randomUUID().toString
      ws.sendOne(GraphQLWSRequest("start", Some(id), Some(self.toGraphQL(useVariables, queryName))))
      new Subscription[(A, Option[Json])] {
        def received: EventStream[Either[CalibanClientError, (A, Option[Json])]] =
          ws.received.collect { case GraphQLWSResponse("data", Some(`id`), Some(payload)) =>
            decode(payload.noSpaces)
          }

        def unsubscribe(): Unit =
          ws.sendOne(GraphQLWSRequest("stop", Some(id), None))
      }
    }

    private def decode(payload: String): Either[CalibanClientError, (A, Option[Json])] =
      for {
        parsed      <- parser
                         .decode[GraphQLResponse](payload)
                         .left
                         .map(ex => DecodingError("Json deserialization error", Some(ex)))
        data        <- if (parsed.errors.nonEmpty) Left(ServerError(parsed.errors)) else Right(parsed.data)
        objectValue <- data match {
                         case Some(o: __ObjectValue) => Right(o)
                         case _                      => Left(DecodingError("Result is not an object"))
                       }
        result      <- self.fromGraphQL(objectValue)
      } yield (result, parsed.extensions)
  }
}
