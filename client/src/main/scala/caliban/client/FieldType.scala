package caliban.client

import caliban.client.CalibanClientError.DecodingError
import caliban.client.ResponseValue._

trait FieldType[+A] {
  def toGraphQL: String
  def fromGraphQL(value: ResponseValue): Either[DecodingError, A]
}

object FieldType {
  case class Scalar[A]()(implicit decoder: ScalarDecoder[A]) extends FieldType[A] {
    override def toGraphQL: String                                           = ""
    override def fromGraphQL(value: ResponseValue): Either[DecodingError, A] = decoder.decode(value)
  }
  case class Obj[Origin, A](selectionSet: SelectionSet[Origin, A]) extends FieldType[A] {
    override def toGraphQL: String = s"{${selectionSet.toGraphQL}}"
    override def fromGraphQL(value: ResponseValue): Either[DecodingError, A] =
      value match {
        case o: ObjectValue => selectionSet.fromGraphQL(o)
        case _              => Left(DecodingError(s"Field $value is not an object"))
      }
  }
  case class ListOf[A](inner: FieldType[A]) extends FieldType[List[A]] {
    override def toGraphQL: String = inner.toGraphQL
    override def fromGraphQL(value: ResponseValue): Either[DecodingError, List[A]] =
      value match {
        case ListValue(items) =>
          items.map(inner.fromGraphQL).foldRight(Right(Nil): Either[DecodingError, List[A]]) { (e, acc) =>
            for (xs <- acc; x <- e) yield x :: xs
          }
        case _ => Left(DecodingError(s"Field $value is not a list"))
      }
  }
  case class OptionOf[A](inner: FieldType[A]) extends FieldType[Option[A]] {
    override def toGraphQL: String = inner.toGraphQL
    override def fromGraphQL(value: ResponseValue): Either[DecodingError, Option[A]] =
      value match {
        case NullValue => Right(None)
        case other     => inner.fromGraphQL(other).map(Some(_))
      }
  }
  case class Union[A](inner: Map[String, FieldType[A]]) extends FieldType[A] {
    override def toGraphQL: String =
      s"{__typename ${inner.map { case (typeName, field) => s"... on $typeName{${field.toGraphQL}}" }.mkString(" ")}}"
    override def fromGraphQL(value: ResponseValue): Either[DecodingError, A] =
      value match {
        case ObjectValue(fields) =>
          for {
            typeNameValue <- fields.find(_._1 == "__typename").map(_._2).toRight(DecodingError("__typename is missing"))
            typeName <- typeNameValue match {
                         case StringValue(value) => Right(value)
                         case _                  => Left(DecodingError("__typename is not a String"))
                       }
            fieldType <- inner.get(typeName).toRight(DecodingError(s"type $typeName is unknown"))
            result    <- fieldType.fromGraphQL(value)
          } yield result
        case _ => Left(DecodingError(s"Field $value is not an object"))
      }
  }
}
