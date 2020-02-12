package caliban.client

import caliban.client.CalibanClientError.DecodingError
import caliban.client.ResponseValue._

trait FieldBuilder[+A] {
  def fromGraphQL(value: ResponseValue): Either[DecodingError, A]
  def toSelectionSet: List[Selection]
}

object FieldBuilder {
  case class Scalar[A]()(implicit decoder: ScalarDecoder[A]) extends FieldBuilder[A] {
    override def fromGraphQL(value: ResponseValue): Either[DecodingError, A] = decoder.decode(value)
    override def toSelectionSet: List[Selection]                             = Nil
  }
  case class Obj[Origin, A](builder: SelectionBuilder[Origin, A]) extends FieldBuilder[A] {
    override def fromGraphQL(value: ResponseValue): Either[DecodingError, A] =
      value match {
        case o: ObjectValue => builder.fromGraphQL(o)
        case _              => Left(DecodingError(s"Field $value is not an object"))
      }
    override def toSelectionSet: List[Selection] = builder.toSelectionSet
  }
  case class ListOf[A](builder: FieldBuilder[A]) extends FieldBuilder[List[A]] {
    override def fromGraphQL(value: ResponseValue): Either[DecodingError, List[A]] =
      value match {
        case ListValue(items) =>
          items.map(builder.fromGraphQL).foldRight(Right(Nil): Either[DecodingError, List[A]]) { (e, acc) =>
            for (xs <- acc; x <- e) yield x :: xs
          }
        case _ => Left(DecodingError(s"Field $value is not a list"))
      }
    override def toSelectionSet: List[Selection] = builder.toSelectionSet
  }
  case class OptionOf[A](builder: FieldBuilder[A]) extends FieldBuilder[Option[A]] {
    override def fromGraphQL(value: ResponseValue): Either[DecodingError, Option[A]] =
      value match {
        case NullValue => Right(None)
        case other     => builder.fromGraphQL(other).map(Some(_))
      }
    override def toSelectionSet: List[Selection] = builder.toSelectionSet
  }
  case class Union[A](builderMap: Map[String, FieldBuilder[A]]) extends FieldBuilder[A] {
    override def fromGraphQL(value: ResponseValue): Either[DecodingError, A] =
      value match {
        case ObjectValue(fields) =>
          for {
            typeNameValue <- fields.find(_._1 == "__typename").map(_._2).toRight(DecodingError("__typename is missing"))
            typeName <- typeNameValue match {
                         case StringValue(value) => Right(value)
                         case _                  => Left(DecodingError("__typename is not a String"))
                       }
            fieldType <- builderMap.get(typeName).toRight(DecodingError(s"type $typeName is unknown"))
            result    <- fieldType.fromGraphQL(value)
          } yield result
        case _ => Left(DecodingError(s"Field $value is not an object"))
      }

    override def toSelectionSet: List[Selection] =
      Selection.Field(None, "__typename", Nil, Nil, Nil) ::
        builderMap.map { case (k, v) => Selection.InlineFragment(k, v.toSelectionSet) }.toList
  }
}
