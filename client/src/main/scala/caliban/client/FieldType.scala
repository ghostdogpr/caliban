package caliban.client

import caliban.ResponseValue
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ NullValue, StringValue }

trait FieldType[+A] {
  def toGraphQL: String
  def fromGraphQL(value: ResponseValue): Either[String, A]
}

object FieldType {
  case class Scalar[A]()(implicit decoder: ScalarDecoder[A]) extends FieldType[A] {
    override def toGraphQL: String                                    = ""
    override def fromGraphQL(value: ResponseValue): Either[String, A] = decoder.decode(value)
  }
  case class Obj[Origin, A](selectionSet: SelectionSet[Origin, A]) extends FieldType[A] {
    override def toGraphQL: String = s"{${selectionSet.toGraphQL}}"
    override def fromGraphQL(value: ResponseValue): Either[String, A] =
      value match {
        case o: ObjectValue => selectionSet.fromGraphQL(o)
        case _              => Left(s"Field is not an object")
      }
  }
  case class ListOf[A](inner: FieldType[A]) extends FieldType[List[A]] {
    override def toGraphQL: String = inner.toGraphQL
    override def fromGraphQL(value: ResponseValue): Either[String, List[A]] =
      value match {
        case ListValue(items) =>
          items.map(inner.fromGraphQL).foldRight(Right(Nil): Either[String, List[A]]) { (e, acc) =>
            for (xs <- acc.right; x <- e.right) yield x :: xs
          }
        case _ => Left(s"Field is not a list")
      }
  }
  case class OptionOf[A](inner: FieldType[A]) extends FieldType[Option[A]] {
    override def toGraphQL: String = inner.toGraphQL
    override def fromGraphQL(value: ResponseValue): Either[String, Option[A]] =
      value match {
        case NullValue => Right(None)
        case other     => inner.fromGraphQL(other).map(Some(_))
      }
  }
  case class Union[A](inner: Map[String, FieldType[A]]) extends FieldType[A] {
    override def toGraphQL: String =
      s"{__typename ${inner.map { case (typeName, field) => s"... on $typeName{${field.toGraphQL}}" }.mkString(" ")}}"
    override def fromGraphQL(value: ResponseValue): Either[String, A] =
      value match {
        case ObjectValue(fields) =>
          for {
            typeNameValue <- fields.find(_._1 == "__typename").map(_._2).toRight("__typename is missing")
            typeName <- typeNameValue match {
                         case StringValue(value) => Right(value)
                         case _                  => Left("__typename is not a String")
                       }
            fieldType <- inner.get(typeName).toRight(s"type $typeName is unknown")
            result    <- fieldType.fromGraphQL(value)
          } yield result
        case _ => Left("Field is not an object")
      }
  }
}
