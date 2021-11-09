package caliban.interop.tapir

import caliban.InputValue.ObjectValue
import caliban.Value.StringValue
import caliban.{ CalibanError, GraphQLRequest, GraphQLWSInput }
import sttp.capabilities.WebSockets
import sttp.capabilities.zio.ZioStreams
import sttp.client3.asynchttpclient.zio._
import sttp.model.{ MediaType, Part, Uri }
import sttp.tapir.{ DecodeResult, WebSocketBodyOutput, WebSocketFrameDecodeFailure }
import sttp.tapir.client.sttp.{ SttpClientInterpreter, WebSocketToPipe }
import sttp.tapir.json.circe._
import sttp.ws.{ WebSocket, WebSocketFrame }
import zio.clock.Clock
import zio.duration._
import zio.stream.{ Stream, ZStream }
import zio.test.Assertion._
import zio.test._
import zio.{ Queue, Task, ZIO }

import scala.language.postfixOps
import scala.reflect.ClassTag

object TapirAdapterSpec {

  // added this to Tapir https://github.com/softwaremill/tapir/pull/1586
  // can delete once it's in Tapir
  implicit private def webSocketToPipe: WebSocketToPipe[ZioStreams with WebSockets] =
    new WebSocketToPipe[ZioStreams with WebSockets] {
      override type S    = ZioStreams
      override type F[X] = Task[X]

      override def apply[REQ, RESP](
        s: Any
      )(ws: WebSocket[F], o: WebSocketBodyOutput[Any, REQ, RESP, _, ZioStreams]): Any = {
        (in: Stream[Throwable, REQ]) =>
          val sends = in
            .map(o.requests.encode)
            .mapM[Any, Throwable, Unit](ws.send(_, isContinuation = false)) // TODO support fragmented frames

          def decode(frame: WebSocketFrame): F[Either[Unit, Option[RESP]]] =
            o.responses.decode(frame) match {
              case failure: DecodeResult.Failure =>
                Task.fail(new WebSocketFrameDecodeFailure(frame, failure))
              case DecodeResult.Value(v)         =>
                Task.right[Option[RESP]](Some(v))
            }

          def raiseBadAccumulator[T](acc: WebSocketFrame, current: WebSocketFrame): F[T] =
            Task.fail(
              new WebSocketFrameDecodeFailure(
                current,
                DecodeResult.Error(
                  "Bad frame sequence",
                  new Exception(
                    s"Invalid accumulator frame: $acc, it can't be concatenated with $current"
                  )
                )
              )
            )

          def concatOrDecode[A <: WebSocketFrame: ClassTag](
            acc: Option[WebSocketFrame],
            frame: A,
            last: Boolean
          )(f: (A, A) => A): F[(Option[WebSocketFrame], Either[Unit, Option[RESP]])] =
            if (last) (acc match {
              case None       => decode(frame)
              case Some(x: A) => decode(f(x, frame))
              case Some(bad)  => raiseBadAccumulator(bad, frame)
            }).map(None -> _)
            else
              (acc match {
                case None       => Task.some(frame)
                case Some(x: A) => Task.some(f(x, frame))
                case Some(bad)  => raiseBadAccumulator(bad, frame)
              }).map(acc => acc -> Left(()))

          val receives = Stream
            .repeatEffect(ws.receive())
            .mapAccumM[Any, Throwable, Option[WebSocketFrame], Either[Unit, Option[RESP]]](
              None
            ) { // left - ignore; right - close or response
              case (acc, _: WebSocketFrame.Close) if !o.decodeCloseResponses =>
                Task.succeed(acc -> Right(None))
              case (acc, _: WebSocketFrame.Pong) if o.ignorePong             =>
                Task.succeed(acc -> Left(()))
              case (acc, WebSocketFrame.Ping(p)) if o.autoPongOnPing         =>
                ws.send(WebSocketFrame.Pong(p)).as(acc -> Left(()))
              case (prev, frame @ WebSocketFrame.Text(_, last, _))           =>
                concatOrDecode(prev, frame, last)((l, r) => r.copy(payload = l.payload + r.payload))
              case (prev, frame @ WebSocketFrame.Binary(_, last, _))         =>
                concatOrDecode(prev, frame, last)((l, r) => r.copy(payload = l.payload ++ r.payload))
              case (_, frame)                                                =>
                Task.fail(
                  new WebSocketFrameDecodeFailure(
                    frame,
                    DecodeResult.Error(
                      "Unrecognised frame type",
                      new Exception(s"Unrecognised frame type: ${frame.getClass}")
                    )
                  )
                )
            }
            .collectRight
            .collectWhileSome

          sends.drain.merge(receives)
      }
    }

  def makeSuite(
    label: String,
    httpUri: Uri,
    uploadUri: Option[Uri] = None,
    wsUri: Option[Uri] = None
  ): ZSpec[Any, Throwable] = {
    val run       =
      SttpClientInterpreter()
        .toRequestThrowDecodeFailures(TapirAdapter.makeHttpEndpoints[Any, CalibanError].head, Some(httpUri))
    val runUpload = uploadUri.map(uploadUri =>
      SttpClientInterpreter()
        .toRequestThrowDecodeFailures(TapirAdapter.makeHttpUploadEndpoint[Any, CalibanError], Some(uploadUri))
    )
    val runWS     = wsUri.map(wsUri =>
      SttpClientInterpreter()
        .toRequestThrowDecodeFailures(TapirAdapter.makeWebSocketEndpoint[Any, CalibanError], Some(wsUri))
    )

    val tests: List[Option[ZSpec[SttpClient, Throwable]]] = List(
      Some(
        testM("test http endpoint") {
          val io =
            for {
              res      <- send(run((GraphQLRequest(Some("{ characters { name }  }")), null)))
              response <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
            } yield response.data.toString

          assertM(io)(
            equalTo(
              """{"characters":[{"name":"James Holden"},{"name":"Naomi Nagata"},{"name":"Amos Burton"},{"name":"Alex Kamal"},{"name":"Chrisjen Avasarala"},{"name":"Josephus Miller"},{"name":"Roberta Draper"}]}"""
            )
          )
        }
      ),
      runUpload.map(runUpload =>
        testM("test http upload endpoint") {
          val query =
            """{ "query": "mutation ($files: [Upload!]!) { uploadFiles(files: $files) { hash, filename, mimetype } }", "variables": { "files": [null, null] }}"""

          val parts =
            List(
              Part("operations", query.getBytes, contentType = Some(MediaType.ApplicationJson)),
              Part("map", """{ "0": ["variables.files.0"], "1":  ["variables.files.1"]}""".getBytes),
              Part("0", """image""".getBytes, contentType = Some(MediaType.ImagePng)),
              Part("1", """text""".getBytes, contentType = Some(MediaType.TextPlain))
            )

          val io =
            for {
              res      <- send(runUpload((parts, null)))
              response <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
            } yield response.data.toString

          assertM(io)(
            equalTo(
              """{"uploadFiles":[{"hash":"6105d6cc76af400325e94d588ce511be5bfdbb73b437dc51eca43917d7a43e3d","filename":"","mimetype":"image/png"},{"hash":"982d9e3eb996f559e633f4d194def3761d909f5a3b647d1a851fead67c32c9d1","filename":"","mimetype":"text/plain"}]}"""
            )
          )
        }
      ),
      runWS.map(runWS =>
        testM("test ws endpoint") {
          val io =
            for {
              res         <- send(runWS(null))
              pipe        <- ZIO.fromEither(res.body).orElseFail(new Throwable("Failed to parse result"))
              inputQueue  <- Queue.unbounded[GraphQLWSInput]
              inputStream  = ZStream.fromQueue(inputQueue)
              outputStream = pipe(inputStream)
              _           <- inputQueue.offer(GraphQLWSInput("connection_init", None, None))
              _           <- inputQueue.offer(
                               GraphQLWSInput(
                                 "start",
                                 Some("id"),
                                 Some(ObjectValue(Map("query" -> StringValue("subscription { characterDeleted }"))))
                               )
                             )
              _           <- send(run((GraphQLRequest(Some("""mutation{ deleteCharacter(name: "Amos Burton") }""")), null)))
                               .delay(2 seconds)
                               .provideSomeLayer[SttpClient](Clock.live)
                               .fork
              messages    <- outputStream.take(2).runCollect
            } yield messages

          io.map { messages =>
            assert(messages.head.`type`)(equalTo("connection_ack")) &&
            assert(messages(1).payload.get.toString)(equalTo("""{"data":{"characterDeleted":"Amos Burton"}}"""))
          }
        }
      )
    )

    suite(label)(tests.flatten: _*)
      .provideLayer(AsyncHttpClientZioBackend.layer().mapError(TestFailure.fail)) @@ TestAspect.sequential
  }
}
