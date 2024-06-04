package caliban.schema

private final case class ProductFieldInfo[R](
  name: String,
  schema: Schema[R, Any],
  index: Int
)
