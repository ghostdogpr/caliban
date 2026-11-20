package caliban.schema

private final case class MethodFieldInfo[R, A](
  name: String,
  annotations: List[Any],
  schema: Schema[R, ?],
  resolve: A => Step[R]
)
