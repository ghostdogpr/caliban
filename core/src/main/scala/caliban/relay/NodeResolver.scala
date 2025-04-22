package caliban.relay

import caliban.CalibanError
import caliban.execution.Field
import caliban.introspection.adt.__Type
import caliban.schema.Step.{ MetadataFunctionStep, QueryStep }
import caliban.schema.{ Schema, Step }
import zio.query.ZQuery

/**
 * A resolver which is used when attempting to materialize different implementations of the `Node` interface
 * @tparam R
 * @tparam ID
 */
trait NodeResolver[-R, ID] {
  def resolve(id: ID): Step[R]
  def toType: __Type
}

object NodeResolver {

  def from[ID]: FromPartiallyApplied[ID] = new FromPartiallyApplied[ID]

  def fromMetadata[ID]: FromMetadataPartiallyApplied[ID] = new FromMetadataPartiallyApplied[ID]

  def fromOption[R, ID, T](
    resolver: ID => Option[T]
  )(implicit schema: Schema[R, T]): NodeResolver[R, ID] =
    new NodeResolver[R, ID] {
      override def resolve(id: ID): Step[R] =
        resolver(id) match {
          case Some(value) => schema.resolve(value)
          case _           => Step.NullStep
        }

      override def toType: __Type = schema.toType_()
    }

  def fromEither[R, ID, T](
    resolver: ID => Either[CalibanError, Option[T]]
  )(implicit schema: Schema[R, T]): NodeResolver[R, ID] =
    new NodeResolver[R, ID] {
      override def resolve(id: ID): Step[R] =
        resolver(id) match {
          case Right(Some(value)) => schema.resolve(value)
          case Right(None)        => Step.NullStep
          case Left(error)        => Step.FailureStep(error)
        }

      override def toType: __Type = schema.toType_()
    }

  def fromZIO[R, ID, T](
    resolver: ID => ZQuery[R, CalibanError, Option[T]]
  )(implicit schema: Schema[R, T]): NodeResolver[R, ID] =
    new NodeResolver[R, ID] {
      override def resolve(id: ID): Step[R] =
        QueryStep(resolver(id).map(_.fold[Step[R]](Step.NullStep)(schema.resolve)))

      override def toType: __Type = schema.toType_()
    }

  def fromQuery[R, ID, T](
    resolver: ID => ZQuery[R, CalibanError, Option[T]]
  )(implicit schema: Schema[R, T]): NodeResolver[R, ID] =
    new NodeResolver[R, ID] {
      override def resolve(id: ID): Step[R] =
        QueryStep(resolver(id).map(_.fold[Step[R]](Step.NullStep)(schema.resolve)))

      override def toType: __Type = schema.toType_()
    }

  final class FromPartiallyApplied[ID](val dummy: Boolean = false) {
    def apply[R, T](
      resolver: ID => ZQuery[R, CalibanError, Option[T]]
    )(implicit schema: Schema[R, T]): NodeResolver[R, ID] = new NodeResolver[R, ID] {
      override def resolve(id: ID): Step[R] =
        QueryStep(resolver(id).map(_.fold[Step[R]](Step.NullStep)(schema.resolve)))

      override def toType: __Type = schema.toType_()
    }
  }

  final class FromMetadataPartiallyApplied[ID](val dummy: Boolean = false) {
    def apply[R, T](
      resolver: Field => ID => ZQuery[R, CalibanError, Option[T]]
    )(implicit schema: Schema[R, T]): NodeResolver[R, ID] = new NodeResolver[R, ID] {
      override def resolve(id: ID): Step[R] =
        MetadataFunctionStep(field =>
          QueryStep(resolver(field)(id).map(_.fold[Step[R]](Step.NullStep)(schema.resolve)))
        )

      override def toType: __Type = schema.toType_()
    }
  }
}
