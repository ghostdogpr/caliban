package caliban.client

import caliban.ResponseValue.ObjectValue
import caliban.client.FieldType.Scalar
import caliban.client.Operations.IsOperation
import caliban.{ GraphQLRequest, GraphQLResponse, GraphQLResponseError, ResponseValue }
import io.circe.parser
import sttp.client._
import sttp.client.circe._
import sttp.model.Uri

sealed trait SelectionSet[-Origin, +A] extends FieldType[A] { self =>
  def ~[Origin1 <: Origin, B](that: SelectionSet[Origin1, B]): SelectionSet[Origin1, (A, B)] =
    SelectionSet.Concat(self, that)
  def map[B](f: A => B): SelectionSet[Origin, B] = SelectionSet.Map(self, f)

  def mapN[B, C, Res](f: (B, C) => Res)(implicit ev: A <:< (B, C)): SelectionSet[Origin, Res] =
    self.map(ev.andThen((v1: (B, C)) => f(v1._1, v1._2)))
  def mapN[B, C, D, Res](f: (B, C, D) => Res)(implicit ev: A <:< ((B, C), D)): SelectionSet[Origin, Res] =
    self.map(ev.andThen((v1: ((B, C), D)) => f(v1._1._1, v1._1._2, v1._2)))
  def mapN[B, C, D, E, Res](f: (B, C, D, E) => Res)(implicit ev: A <:< (((B, C), D), E)): SelectionSet[Origin, Res] =
    self.map(ev.andThen((v1: (((B, C), D), E)) => f(v1._1._1._1, v1._1._1._2, v1._1._2, v1._2)))

  def toRequest[A1 >: A, Origin1 <: Origin](
    uri: Uri
  )(implicit ev: IsOperation[Origin1]): Request[Either[Exception, A1], Nothing] = {
    val operation = s"${ev.operationName}{$toGraphQL}"

    basicRequest
      .post(uri)
      .body(GraphQLRequest(operation, None, None))
      .mapResponse { response =>
        for {
          resp   <- response.left.map(new Exception(_))
          parsed <- parser.decode[GraphQLResponse[GraphQLResponseError]](resp)
          objectValue <- parsed.data match {
                          case o: ObjectValue => Right(o)
                          case _              => Left(new Exception("Result is now an object"))
                        }
          result <- fromGraphQL(objectValue).left.map(new Exception(_))
        } yield result
      }
  }
}

object SelectionSet {

  val __typename: SelectionSet[Any, String] = Field("__typename", Scalar[String]())

  case class Field[Origin, A](name: String, field: FieldType[A], arguments: List[Argument[_]] = Nil)
      extends SelectionSet[Origin, A] {
    override def toGraphQL: String = {
      val args = arguments.map(_.toGraphQL).filterNot(_.isEmpty).mkString(",")
      if (args.nonEmpty) s"$name($args) ${field.toGraphQL}"
      else name + field.toGraphQL
    }
    override def fromGraphQL(value: ResponseValue): Either[String, A] =
      value match {
        case ObjectValue(fields) =>
          fields.find(_._1 == name).toRight(s"Missing field $name").flatMap(v => field.fromGraphQL(v._2))
        case _ => Left(s"Invalid field type $name")
      }
  }
  case class Concat[Origin, A, B](first: SelectionSet[Origin, A], second: SelectionSet[Origin, B])
      extends SelectionSet[Origin, (A, B)] {
    override def toGraphQL: String = s"${first.toGraphQL} ${second.toGraphQL}"
    override def fromGraphQL(value: ResponseValue): Either[String, (A, B)] =
      for {
        v1 <- first.fromGraphQL(value)
        v2 <- second.fromGraphQL(value)
      } yield (v1, v2)
  }
  case class Map[Origin, A, B](selectionSet: SelectionSet[Origin, A], f: A => B) extends SelectionSet[Origin, B] {
    override def toGraphQL: String                                    = selectionSet.toGraphQL
    override def fromGraphQL(value: ResponseValue): Either[String, B] = selectionSet.fromGraphQL(value).map(f)
  }
//  case class GraphQL[Origin, A](query: String, read: ResponseValue => Either[String, A])
//      extends SelectionSet[Origin, A] {
//    override def toGraphQL: String                                    = query
//    override def fromGraphQL(value: ResponseValue): Either[String, A] = read(value)
//  }
}
