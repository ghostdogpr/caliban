package caliban

import caliban.CalibanError.ValidationError
import caliban.ResponseValue.{ ObjectValue, StreamValue }
import caliban.Value.NullValue
import caliban.wrappers.Caching
import zio.stream.{ UStream, ZChannel, ZPipeline, ZStream }
import zio.{ Cause, Chunk, Trace }

import scala.util.Try

object HttpUtils {

  val MutationOverGetError: ValidationError =
    ValidationError("Mutations are not allowed for GET requests", "")

  private[caliban] val SubscriptionOverJsonError: ResponseValue =
    GraphQLResponse(
      NullValue,
      List(CalibanError.ExecutionError("Subscriptions require text/event-stream or WebSocket."))
    ).toResponseValue

  private[caliban] sealed trait Delivery

  private[caliban] object Delivery {
    final case class Incremental(responses: ZStream[Any, Throwable, ResponseValue]) extends Delivery
    final case class Subscription(events: ZStream[Any, Throwable, ResponseValue], failed: Throwable => ResponseValue)
        extends Delivery {
      def recovered(implicit trace: Trace): UStream[ResponseValue] =
        events.catchAll(error => ZStream.succeed(failed(error)))
    }
    case object Single extends Delivery
  }

  private[caliban] def delivery(response: GraphQLResponse[Any], outcome: GraphQLResponseContext.Outcome)(implicit
    trace: Trace
  ): Delivery = {
    // Report errors in an initial event sent immediately.
    def withErrors(events: ZStream[Any, Throwable, ResponseValue]) =
      if (response.errors.isEmpty) events
      else ZStream.succeed(GraphQLResponse(NullValue, response.errors).toResponseValue) ++ events
    response.data match {
      case StreamValue(stream) if outcome ne GraphQLResponseContext.Outcome.Subscribed =>
        Delivery.Incremental(stream.via(DeferMultipart.createPipeline(response)))
      case StreamValue(stream)                                                         =>
        Delivery.Subscription(withErrors(stream), e => GraphQLResponse(NullValue, List(e)).toResponseValue)
      case ObjectValue((fieldName, StreamValue(stream)) :: Nil)                        =>
        def event(value: ResponseValue, errors: List[Throwable]) =
          GraphQLResponse(ObjectValue(List(fieldName -> value)), errors).toResponseValue
        Delivery.Subscription(withErrors(stream.map(event(_, Nil))), error => event(NullValue, List(error)))
      case _                                                                           => Delivery.Single
    }
  }

  object DeferMultipart {
    private val Newline        = "\r\n"
    private val ContentType    = "Content-Type: application/json; charset=utf-8"
    private val SubHeader      = s"$Newline$ContentType$Newline$Newline"
    private val Boundary       = "---"
    private val BoundaryHeader = "-"
    private val DeferSpec      = "20220824"

    val InnerBoundary = s"$Newline$Boundary$SubHeader"
    val EndBoundary   = s"$Newline-----$Newline"

    val DeferHeaderParams: Map[String, String] = Map("boundary" -> BoundaryHeader, "deferSpec" -> DeferSpec)

    /**
     * `InnerBoundary` and `EndBoundary` split into the delimiter that ends a part and what
     * follows it: `PartDelimiter + PartHeader == InnerBoundary`,
     * `PartDelimiter + CloseDelimiter == EndBoundary`. Writing `PartDelimiter` right after each
     * part lets a multipart parser surface that part immediately, while the stream's completion
     * decides whether the next part opens or the body closes.
     */
    val PartDelimiter  = s"$Newline$Boundary"
    val PartHeader     = SubHeader
    val CloseDelimiter = s"--$Newline"

    def createPipeline[E](resp: GraphQLResponse[E]): ZPipeline[Any, Throwable, ResponseValue, ResponseValue] =
      ZPipeline.fromChannel {
        lazy val reader: ZChannel[Any, Throwable, Chunk[ResponseValue], Any, Throwable, Chunk[ResponseValue], Any] =
          ZChannel.readWithCause(
            (in: Chunk[ResponseValue]) =>
              in.headOption match {
                case Some(value) =>
                  ZChannel.write(in.updated(0, resp.copy(data = value).toResponseValue)) *>
                    ZChannel.identity[Throwable, Chunk[ResponseValue], Any]
                case None        => reader
              },
            (cause: Cause[Throwable]) => ZChannel.failCause(cause),
            (_: Any) => ZChannel.unit
          )

        reader
      }
  }

  object ServerSentEvents {

    def transformResponse[Sse](
      resp: GraphQLResponse[Any],
      toSse: ResponseValue => Sse,
      done: Sse,
      heartbeater: Option[ZStream[Any, Nothing, Sse]] = None
    )(implicit trace: Trace): UStream[Sse] = {
      val events = delivery(resp, GraphQLResponseContext.Outcome.Subscribed) match {
        case subscription: Delivery.Subscription => subscription.recovered
        case _                                   => ZStream.succeed(resp.toResponseValue)
      }
      fromEvents(events, toSse, done, heartbeater)
    }

    private[caliban] def fromEvents[Sse](
      events: UStream[ResponseValue],
      toSse: ResponseValue => Sse,
      done: Sse,
      heartbeater: Option[ZStream[Any, Nothing, Sse]]
    )(implicit trace: Trace): UStream[Sse] = {
      val stream = events.map(toSse)
      (heartbeater match {
        case None    => stream
        case Some(s) => stream.mergeHaltLeft(s)
      }) ++ ZStream.succeed(done)
    }
  }

  def computeCacheDirective(extensions: ResponseValue.ObjectValue): Option[String] =
    extensions.fields.collectFirst { case (Caching.DirectiveName, ResponseValue.ObjectValue(fields)) =>
      fields.collectFirst { case ("httpHeader", Value.StringValue(cacheHeader)) => cacheHeader }
    }.flatten

  private[caliban] sealed abstract class ResponseEncoding(
    val mainType: String,
    val subType: String,
    val parameters: Map[String, String],
    val strict: Boolean
  )

  private[caliban] object ResponseEncoding {
    case object GraphQLJson extends ResponseEncoding("application", "graphql-response+json", Map.empty, strict = true)
    case object Json        extends ResponseEncoding("application", "json", Map.empty, strict = false)
    case object EventStream extends ResponseEncoding("text", "event-stream", Map.empty, strict = false)
    case object Multipart
        extends ResponseEncoding("multipart", "mixed", DeferMultipart.DeferHeaderParams, strict = true)

    private val supported = List(GraphQLJson, Json, EventStream, Multipart)

    def negotiate(accept: Seq[String]): Option[ResponseEncoding] =
      fieldValue(accept).map(_.trim).filter(_.nonEmpty) match {
        case None                                                            => Some(Json)
        case Some(a) if a == "*/*" || a.equalsIgnoreCase("application/json") => Some(Json)
        case Some(a)                                                         =>
          val ranges = parse(a)
          best(ranges, supported).orElse(
            if (
              ranges.exists(r =>
                r.mainType.equalsIgnoreCase("application") && r.subType.equalsIgnoreCase("json") && r.q > 0d
              )
            ) Some(Json)
            else None
          )
      }

    def acceptsEventStream(accept: Seq[String]): Boolean =
      fieldValue(accept).exists(a => best(parse(a), EventStream :: Nil).isDefined)

    private def fieldValue(lines: Seq[String]): Option[String] =
      if (lines.lengthCompare(1) > 0) Some(lines.mkString(",")) else lines.headOption

    private final case class Range(mainType: String, subType: String, parameters: List[(String, String)], q: Double)

    private final case class Negotiated(value: ResponseEncoding, q: Double, specificity: Int) {
      def isPreferredOver(other: Negotiated): Boolean =
        q > other.q || q == other.q &&
          (specificity > other.specificity || specificity == 0 && other.specificity == 0 && value == Json)
    }

    private def best(ranges: List[Range], candidates: List[ResponseEncoding]): Option[ResponseEncoding] =
      candidates.flatMap { candidate =>
        ranges
          .filter(matches(candidate, _))
          .reduceOption((best, range) => if (specificity(range) > specificity(best)) range else best)
          .filter(_.q > 0d)
          .map(range => Negotiated(candidate, range.q, typeSpecificity(range)))
      }.reduceOption((current, candidate) => if (candidate.isPreferredOver(current)) candidate else current)
        .map(_.value)

    private def parse(accept: String): List[Range] =
      accept.split(',').toList.flatMap { element =>
        val (range, rest)   = element.span(_ != ';')
        val mediaRange      = range.trim
        val slash           = mediaRange.indexOf('/')
        val multipart       = mediaRange.regionMatches(true, 0, "multipart/", 0, 10)
        val (q, parameters) = rest
          .split(';')
          .toList
          .map(_.trim)
          .collect {
            case parameter if parameter.nonEmpty =>
              val eq = parameter.indexOf('=')
              if (eq < 0) parameter -> ""
              else {
                val name  = parameter.substring(0, eq).trim
                val value = parameter.substring(eq + 1).trim
                name -> (if (name.equalsIgnoreCase("q")) value else unquote(value))
              }
          }
          .filterNot(p => multipart && p._1.equalsIgnoreCase("boundary"))
          .partition(_._1.equalsIgnoreCase("q"))
        val quality         = q.headOption
          .map(_._2)
          .filter(v => v.length == 1 || v.length <= 5 && v.lastIndexOf('.') == 1)
          .filter(_.forall(c => c == '.' || c >= '0' && c <= '9'))
          .flatMap(v => Try(v.toDouble).toOption)
          .filter(_ <= 1d)
          .getOrElse(1d)
        if (slash <= 0 || slash == mediaRange.length - 1) None
        else Some(Range(mediaRange.substring(0, slash), mediaRange.substring(slash + 1), parameters, quality))
      }

    private def matches(candidate: ResponseEncoding, range: Range): Boolean =
      (range.mainType == "*" || range.mainType.equalsIgnoreCase(candidate.mainType)) &&
        (range.subType == "*" || range.subType.equalsIgnoreCase(candidate.subType)) &&
        range.parameters.forall {
          case (name, value) if name.equalsIgnoreCase("charset") =>
            value.equalsIgnoreCase("utf-8") || value.equalsIgnoreCase("utf8")
          case (name, value)                                     =>
            candidate.parameters.exists { case (k, v) => k.equalsIgnoreCase(name) && v.equalsIgnoreCase(value) }
        }

    private def typeSpecificity(range: Range): Int =
      if (range.mainType == "*") 0 else if (range.subType == "*") 1 else 2

    private def specificity(range: Range): Int =
      typeSpecificity(range) * 100 + range.parameters.size

    private def unquote(value: String): String =
      if (value.length >= 2 && value.head == '"' && value.last == '"') value.substring(1, value.length - 1)
      else value
  }

  private[caliban] final class HttpStatus(val code: Int, val encoding: ResponseEncoding, val keepDataOnErrors: Boolean)

  private[caliban] def httpStatus(
    response: GraphQLResponse[Any],
    outcome: GraphQLResponseContext.Outcome,
    encoding: ResponseEncoding,
    partialSuccessStatus: Boolean = false,
    jsonClientStatusCodes: Boolean = false
  ): HttpStatus = {
    import GraphQLResponseContext.{ Outcome, ServerFailure }
    val strict  = encoding.strict || encoding == ResponseEncoding.Json && jsonClientStatusCodes
    val code    = outcome match {
      case ServerFailure.Internal                             => 500
      case ServerFailure.Unavailable                          => 503
      case ServerFailure.TimedOut                             => 504
      case Outcome.MutationOverGet                            => 405
      case Outcome.ParsingFailed | Outcome.Rejected if strict => 400
      case Outcome.ValidationFailed if strict                 => 422
      case Outcome.Executed
          if partialSuccessStatus && encoding == ResponseEncoding.GraphQLJson && response.errors.nonEmpty =>
        294
      case _                                                  => 200
    }
    val success = code >= 200 && code < 300
    val body    = if (strict && !success && encoding == ResponseEncoding.Json) ResponseEncoding.GraphQLJson else encoding
    new HttpStatus(code, body, success || !strict)
  }

  def graphiqlHtml(apiPath: String, uiPath: String, wsPath: Option[String]): String =
    s"""|<!doctype html>
        |<html lang="en">
        |  <head>
        |    <title>GraphiQL</title>
        |    <style>
        |      body {
        |        height: 100%;
        |        margin: 0;
        |        width: 100%;
        |        overflow: hidden;
        |      }
        |
        |      #graphiql {
        |        height: 100vh;
        |      }
        |    </style>
        |    <link rel="stylesheet" href="https://esm.sh/graphiql/dist/style.css" />
        |    <script type="importmap">
        |        {
        |            "imports": {
        |                "react": "https://esm.sh/react@19.1.0",
        |                "react/jsx-runtime": "https://esm.sh/react@19.1.0/jsx-runtime",
        |                "react-dom": "https://esm.sh/react-dom@19.1.0",
        |                "react-dom/client": "https://esm.sh/react-dom@19.1.0/client",
        |                "graphiql": "https://esm.sh/graphiql?standalone&external=react,react-dom,@graphiql/react,graphql",
        |                "@graphiql/react": "https://esm.sh/@graphiql/react?standalone&external=react,react-dom,graphql",
        |                "@graphiql/toolkit": "https://esm.sh/@graphiql/toolkit?standalone&external=graphql",
        |                "graphql": "https://esm.sh/graphql@16.11.0",
        |                "regenerator-runtime/runtime": "https://esm.sh/regenerator-runtime/runtime"
        |            }
        |        }
        |    </script>
        |</head>
        |
        |<body>
        |    <div id="graphiql">Loading...</div>
        |    <script type="module">
        |        import React from 'react';
        |        import ReactDOM from 'react-dom/client';
        |        import { GraphiQL, HISTORY_PLUGIN } from 'graphiql';
        |        import { createGraphiQLFetcher } from '@graphiql/toolkit';
        |        import 'regenerator-runtime/runtime';
        |        import createJSONWorker from 'https://esm.sh/monaco-editor/esm/vs/language/json/json.worker.js?worker';
        |        import createGraphQLWorker from 'https://esm.sh/monaco-graphql/esm/graphql.worker.js?worker';
        |        import createEditorWorker from 'https://esm.sh/monaco-editor/esm/vs/editor/editor.worker.js?worker';
        |
        |        globalThis.MonacoEnvironment = {
        |            getWorker(_workerId, label) {
        |                console.info('MonacoEnvironment.getWorker', { label });
        |                switch (label) {
        |                    case 'json':
        |                        return createJSONWorker();
        |                    case 'graphql':
        |                        return createGraphQLWorker();
        |                }
        |                return createEditorWorker();
        |            },
        |        };
        |
        |        const fetcher = createGraphiQLFetcher({
        |            url: window.location.href.replace("$uiPath", "$apiPath"),
        |        ${wsPath.fold("") { wsPath =>
         s"subscriptionUrl: window.location.href.replace(\'$uiPath\', \'$wsPath\').replace(\'https:\',\'wss:\').replace(\'http:\',\'ws:\')"
       }}
        |        });
        |        const plugins = [HISTORY_PLUGIN];
        |
        |        function App() {
        |            return React.createElement(GraphiQL, {
        |                fetcher,
        |                plugins,
        |                defaultEditorToolsVisibility: true,
        |            });
        |        }
        |        const root = ReactDOM.createRoot(document.getElementById('graphiql'));
        |        root.render(React.createElement(App));
        |    </script>
        |  </body>
        |</html>
        |""".stripMargin
}
