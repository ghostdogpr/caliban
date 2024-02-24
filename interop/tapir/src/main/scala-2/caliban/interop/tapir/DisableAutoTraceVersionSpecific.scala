package caliban.interop.tapir

import zio.stacktracer.{ DisableAutoTrace, TracingImplicits }

// See https://github.com/zio/zio-http/issues/2700
private object DisableAutoTraceVersionSpecific {
  implicit val disableAutoTrace: DisableAutoTrace = TracingImplicits.disableAutoTrace
}
