package caliban

import zio.Trace
import zio.http.{ HandlerAspect, RequestHandler }
import zio.stacktracer.TracingImplicits.disableAutoTrace

final case class QuickHandlers[-R](
  api: RequestHandler[R, Nothing],
  upload: RequestHandler[R, Nothing],
  webSocket: RequestHandler[R, Nothing]
) {

  /**
   * Applies a ZIO HTTP `HandlerAspect` to both the api and upload handlers
   */
  def @@[R1 <: R](aspect: HandlerAspect[R1, Unit]): QuickHandlers[R1] = {
    implicit val trace: Trace = Trace.empty
    QuickHandlers(
      api = (api @@ aspect).merge,
      upload = (upload @@ aspect).merge,
      webSocket = (webSocket @@ aspect).merge
    )
  }

}
