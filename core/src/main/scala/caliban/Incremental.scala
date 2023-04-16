package caliban

import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ BooleanValue, StringValue }

sealed trait Incremental[+E] extends Product with Serializable {
  def path: ListValue
  def errors: List[E]
  def label: Option[String]
  def extensions: Option[ObjectValue]

  def toResponseValue: ResponseValue
}

object Incremental {

  case class Stream[+E](
    items: List[ResponseValue],
    path: ListValue,
    errors: List[E],
    label: Option[String],
    extensions: Option[ObjectValue] = None
  ) extends Incremental[E] {

    def toResponseValue: ResponseValue =
      ObjectValue(
        List(
          "items"      -> Some(ListValue(items)),
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

  case class Defer[+E](
    data: ResponseValue,
    path: ListValue,
    errors: List[E],
    label: Option[String],
    extensions: Option[ObjectValue] = None
  ) extends Incremental[E] {

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
