package caliban

import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ BooleanValue, StringValue }

case class Incremental[+E](
  data: ResponseValue,
  errors: List[E],
  path: ListValue,
  label: Option[String],
  extensions: Option[ObjectValue] = None
) {

  def toResponseValue: ResponseValue =
    ObjectValue(
      List(
        "data"       -> Some(data),
        "errors"     -> (if (errors.nonEmpty)
                       Some(ListValue(errors.map {
                         case e: CalibanError => e.toResponseValue
                         case e               => ObjectValue(List("message" -> StringValue(e.toString)))
                       }))
                     else None),
        "extensions" -> extensions,
        "path"       -> Some(path),
        "label"      -> label.map(StringValue.apply)
      ).collect { case (name, Some(v)) => name -> v }
    )

}

case class GraphQLIncrementalResponse[+E](
  incremental: List[Incremental[E]],
  hasNext: Boolean
) {

  def toResponseValue: ResponseValue =
    ObjectValue(
      List(
        "incremental" -> (if (incremental.nonEmpty)
                            Some(ListValue(incremental.map(_.toResponseValue)))
                          else None),
        "hasNext"     -> Some(BooleanValue(hasNext))
      ).collect { case (name, Some(v)) => name -> v }
    )
}

object GraphQLIncrementalResponse {
  val empty: GraphQLIncrementalResponse[Nothing] = GraphQLIncrementalResponse(Nil, hasNext = false)
}
