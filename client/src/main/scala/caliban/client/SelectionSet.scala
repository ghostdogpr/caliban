package caliban.client

import caliban.client.CalibanClientError.{ CommunicationError, DecodingError }
import caliban.client.FieldType.Scalar
import caliban.client.Operations.IsOperation
import caliban.client.ResponseValue.ObjectValue
import io.circe.parser
import sttp.client._
import sttp.client.circe._
import sttp.model.Uri

sealed trait SelectionSet[-Origin, +A] extends FieldType[A] { self =>
  def ~[Origin1 <: Origin, B](that: SelectionSet[Origin1, B]): SelectionSet[Origin1, (A, B)] =
    SelectionSet.Concat(self, that)
  def map[B](f: A => B): SelectionSet[Origin, B] = SelectionSet.Map(self, f)

  def toRequest[A1 >: A, Origin1 <: Origin](
    uri: Uri
  )(implicit ev: IsOperation[Origin1]): Request[Either[CalibanClientError, A1], Nothing] = {
    val operation = s"${ev.operationName}{$toGraphQL}"

    basicRequest
      .post(uri)
      .body(GraphQLRequest(operation))
      .mapResponse { response =>
        for {
          resp <- response.left.map(CommunicationError(_))
          parsed <- parser
                     .decode[GraphQLResponse](resp)
                     .left
                     .map(ex => DecodingError("Json deserialization error", Some(ex)))
          objectValue <- parsed.data match {
                          case o: ObjectValue => Right(o)
                          case _              => Left(DecodingError("Result is not an object"))
                        }
          result <- fromGraphQL(objectValue)
        } yield result
      }
  }

  def mapN[B, C, Res](f: (B, C) => Res)(implicit ev: A <:< (B, C)): SelectionSet[Origin, Res] =
    self.map(ev.andThen { case (b, c) => f(b, c) })
  def mapN[B, C, D, Res](f: (B, C, D) => Res)(implicit ev: A <:< ((B, C), D)): SelectionSet[Origin, Res] =
    self.map(ev.andThen { case ((b, c), d) => f(b, c, d) })
  def mapN[B, C, D, E, Res](f: (B, C, D, E) => Res)(implicit ev: A <:< (((B, C), D), E)): SelectionSet[Origin, Res] =
    self.map(ev.andThen { case (((b, c), d), e) => f(b, c, d, e) })
  def mapN[B, C, D, E, F, Res](
    f: (B, C, D, E, F) => Res
  )(implicit ev: A <:< ((((B, C), D), E), F)): SelectionSet[Origin, Res] =
    self.map(ev.andThen { case ((((b, c), d), e), ff) => f(b, c, d, e, ff) })
  def mapN[B, C, D, E, F, G, Res](
    f: (B, C, D, E, F, G) => Res
  )(implicit ev: A <:< (((((B, C), D), E), F), G)): SelectionSet[Origin, Res] =
    self.map(ev.andThen { case (((((b, c), d), e), ff), g) => f(b, c, d, e, ff, g) })
  def mapN[B, C, D, E, F, G, H, Res](
    f: (B, C, D, E, F, G, H) => Res
  )(implicit ev: A <:< ((((((B, C), D), E), F), G), H)): SelectionSet[Origin, Res] =
    self.map(ev.andThen { case ((((((b, c), d), e), ff), g), h) => f(b, c, d, e, ff, g, h) })
  def mapN[B, C, D, E, F, G, H, I, Res](
    f: (B, C, D, E, F, G, H, I) => Res
  )(implicit ev: A <:< (((((((B, C), D), E), F), G), H), I)): SelectionSet[Origin, Res] =
    self.map(ev.andThen { case (((((((b, c), d), e), ff), g), h), i) => f(b, c, d, e, ff, g, h, i) })
  def mapN[B, C, D, E, F, G, H, I, J, Res](
    f: (B, C, D, E, F, G, H, I, J) => Res
  )(implicit ev: A <:< ((((((((B, C), D), E), F), G), H), I), J)): SelectionSet[Origin, Res] =
    self.map(ev.andThen { case ((((((((b, c), d), e), ff), g), h), i), j) => f(b, c, d, e, ff, g, h, i, j) })
  def mapN[B, C, D, E, F, G, H, I, J, K, Res](
    f: (B, C, D, E, F, G, H, I, J, K) => Res
  )(implicit ev: A <:< (((((((((B, C), D), E), F), G), H), I), J), K)): SelectionSet[Origin, Res] =
    self.map(ev.andThen { case (((((((((b, c), d), e), ff), g), h), i), j), k) => f(b, c, d, e, ff, g, h, i, j, k) })
  def mapN[B, C, D, E, F, G, H, I, J, K, L, Res](
    f: (B, C, D, E, F, G, H, I, J, K, L) => Res
  )(implicit ev: A <:< ((((((((((B, C), D), E), F), G), H), I), J), K), L)): SelectionSet[Origin, Res] =
    self.map(ev.andThen {
      case ((((((((((b, c), d), e), ff), g), h), i), j), k), l) => f(b, c, d, e, ff, g, h, i, j, k, l)
    })
  def mapN[B, C, D, E, F, G, H, I, J, K, L, M, Res](
    f: (B, C, D, E, F, G, H, I, J, K, L, M) => Res
  )(implicit ev: A <:< (((((((((((B, C), D), E), F), G), H), I), J), K), L), M)): SelectionSet[Origin, Res] =
    self.map(ev.andThen {
      case (((((((((((b, c), d), e), ff), g), h), i), j), k), l), m) => f(b, c, d, e, ff, g, h, i, j, k, l, m)
    })
  def mapN[B, C, D, E, F, G, H, I, J, K, L, M, N, Res](
    f: (B, C, D, E, F, G, H, I, J, K, L, M, N) => Res
  )(implicit ev: A <:< ((((((((((((B, C), D), E), F), G), H), I), J), K), L), M), N)): SelectionSet[Origin, Res] =
    self.map(ev.andThen {
      case ((((((((((((b, c), d), e), ff), g), h), i), j), k), l), m), n) => f(b, c, d, e, ff, g, h, i, j, k, l, m, n)
    })
  def mapN[B, C, D, E, F, G, H, I, J, K, L, M, N, O, Res](
    f: (B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Res
  )(implicit ev: A <:< (((((((((((((B, C), D), E), F), G), H), I), J), K), L), M), N), O)): SelectionSet[Origin, Res] =
    self.map(ev.andThen {
      case (((((((((((((b, c), d), e), ff), g), h), i), j), k), l), m), n), o) =>
        f(b, c, d, e, ff, g, h, i, j, k, l, m, n, o)
    })
  def mapN[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Res](
    f: (B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Res
  )(
    implicit ev: A <:< ((((((((((((((B, C), D), E), F), G), H), I), J), K), L), M), N), O), P)
  ): SelectionSet[Origin, Res] =
    self.map(ev.andThen {
      case ((((((((((((((b, c), d), e), ff), g), h), i), j), k), l), m), n), o), p) =>
        f(b, c, d, e, ff, g, h, i, j, k, l, m, n, o, p)
    })
  def mapN[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Res](
    f: (B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Res
  )(
    implicit ev: A <:< (((((((((((((((B, C), D), E), F), G), H), I), J), K), L), M), N), O), P), Q)
  ): SelectionSet[Origin, Res] =
    self.map(ev.andThen {
      case (((((((((((((((b, c), d), e), ff), g), h), i), j), k), l), m), n), o), p), q) =>
        f(b, c, d, e, ff, g, h, i, j, k, l, m, n, o, p, q)
    })
  def mapN[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Res](
    f: (B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Res
  )(
    implicit ev: A <:< ((((((((((((((((B, C), D), E), F), G), H), I), J), K), L), M), N), O), P), Q), R)
  ): SelectionSet[Origin, Res] =
    self.map(ev.andThen {
      case ((((((((((((((((b, c), d), e), ff), g), h), i), j), k), l), m), n), o), p), q), r) =>
        f(b, c, d, e, ff, g, h, i, j, k, l, m, n, o, p, q, r)
    })
  def mapN[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Res](
    f: (B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Res
  )(
    implicit ev: A <:< (((((((((((((((((B, C), D), E), F), G), H), I), J), K), L), M), N), O), P), Q), R), S)
  ): SelectionSet[Origin, Res] =
    self.map(ev.andThen {
      case (((((((((((((((((b, c), d), e), ff), g), h), i), j), k), l), m), n), o), p), q), r), s) =>
        f(b, c, d, e, ff, g, h, i, j, k, l, m, n, o, p, q, r, s)
    })
  def mapN[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Res](
    f: (B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Res
  )(
    implicit ev: A <:< ((((((((((((((((((B, C), D), E), F), G), H), I), J), K), L), M), N), O), P), Q), R), S), T)
  ): SelectionSet[Origin, Res] =
    self.map(ev.andThen {
      case ((((((((((((((((((b, c), d), e), ff), g), h), i), j), k), l), m), n), o), p), q), r), s), t) =>
        f(b, c, d, e, ff, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
    })
  def mapN[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Res](
    f: (B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Res
  )(
    implicit ev: A <:< (((((((((((((((((((B, C), D), E), F), G), H), I), J), K), L), M), N), O), P), Q), R), S), T), U)
  ): SelectionSet[Origin, Res] =
    self.map(ev.andThen {
      case (((((((((((((((((((b, c), d), e), ff), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u) =>
        f(b, c, d, e, ff, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
    })
  def mapN[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Res](
    f: (B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Res
  )(
    implicit ev: A <:< (
      (((((((((((((((((((B, C), D), E), F), G), H), I), J), K), L), M), N), O), P), Q), R), S), T), U),
      V
    )
  ): SelectionSet[Origin, Res] =
    self.map(ev.andThen {
      case ((((((((((((((((((((b, c), d), e), ff), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u), v) =>
        f(b, c, d, e, ff, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
    })
  def mapN[B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, Res](
    f: (B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) => Res
  )(
    implicit ev: A <:< (
      ((((((((((((((((((((B, C), D), E), F), G), H), I), J), K), L), M), N), O), P), Q), R), S), T), U), V),
      W
    )
  ): SelectionSet[Origin, Res] =
    self.map(ev.andThen {
      case (
          ((((((((((((((((((((b, c), d), e), ff), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u), v),
          w
          ) =>
        f(b, c, d, e, ff, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)
    })
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
    override def fromGraphQL(value: ResponseValue): Either[DecodingError, A] =
      value match {
        case ObjectValue(fields) =>
          fields.find(_._1 == name).toRight(DecodingError(s"Missing field $name")).flatMap(v => field.fromGraphQL(v._2))
        case _ => Left(DecodingError(s"Invalid field type $name"))
      }
  }
  case class Concat[Origin, A, B](first: SelectionSet[Origin, A], second: SelectionSet[Origin, B])
      extends SelectionSet[Origin, (A, B)] {
    override def toGraphQL: String = s"${first.toGraphQL} ${second.toGraphQL}"
    override def fromGraphQL(value: ResponseValue): Either[DecodingError, (A, B)] =
      for {
        v1 <- first.fromGraphQL(value)
        v2 <- second.fromGraphQL(value)
      } yield (v1, v2)
  }
  case class Map[Origin, A, B](selectionSet: SelectionSet[Origin, A], f: A => B) extends SelectionSet[Origin, B] {
    override def toGraphQL: String                                           = selectionSet.toGraphQL
    override def fromGraphQL(value: ResponseValue): Either[DecodingError, B] = selectionSet.fromGraphQL(value).map(f)
  }
}
