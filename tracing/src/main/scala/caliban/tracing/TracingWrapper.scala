package caliban.tracing

object TracingWrapper {
  val traced = SchemaTracer.wrapper @@ FieldTracer.wrapper
}
