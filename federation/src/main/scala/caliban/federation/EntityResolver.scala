package caliban.federation

import caliban.execution.Field
import caliban.introspection.adt.__Type
import caliban.schema.Step.QueryStep
import caliban.schema.{ ArgBuilder, Schema, Step }
import caliban.{ CalibanError, InputValue }
import zio.ZIO
import zio.query.ZQuery

/**
 * A resolver which is used when attempting to materialize types from their "any" representation
 */
trait EntityResolver[-R] {
  def resolve(value: InputValue): Step[R]
  def toType: __Type
}

object EntityResolver {
  def apply[R, A: ArgBuilder, T](
    resolver: A => ZQuery[R, CalibanError, Option[T]]
  )(implicit schema: Schema[R, T]): EntityResolver[R] =
    new EntityResolver[R] {
      override def resolve(value: InputValue): Step[R] =
        QueryStep(
          ZQuery.fromEither(implicitly[ArgBuilder[A]].build(value)).flatMap { arg =>
            resolver(arg).map(_.fold[Step[R]](Step.NullStep)(schema.resolve))
          }
        )

      override def toType: __Type = schema.toType_()
    }

  def fromFunction[A: ArgBuilder, T](
    resolver: A => Option[T]
  )(implicit schema: Schema[Any, T]): EntityResolver[Any] =
    new EntityResolver[Any] {
      override def resolve(value: InputValue): Step[Any] =
        QueryStep(
          ZQuery.fromEither(implicitly[ArgBuilder[A]].build(value)).map { arg =>
            resolver(arg).fold[Step[Any]](Step.NullStep)(schema.resolve)
          }
        )

      override def toType: __Type = schema.toType_()
    }

  def fromZIO[R, A: ArgBuilder, T](
    resolver: A => ZIO[R, CalibanError, Option[T]]
  )(implicit schema: Schema[R, T]): EntityResolver[R] =
    new EntityResolver[R] {
      override def resolve(value: InputValue): Step[R] =
        QueryStep(
          ZQuery.fromZIO(
            ZIO.fromEither(implicitly[ArgBuilder[A]].build(value)).flatMap { arg =>
              resolver(arg).map(_.fold[Step[R]](Step.NullStep)(schema.resolve))
            }
          )
        )

      override def toType: __Type = schema.toType_()
    }

  def from[A]: EntityResolverPartiallyApplied[A] =
    new EntityResolverPartiallyApplied

  def fromMetadata[A]: MetadataEntityResolverPartiallyApplied[A] =
    new MetadataEntityResolverPartiallyApplied[A]

  class MetadataEntityResolverPartiallyApplied[A](val dummy: Boolean = false) extends AnyVal {
    def apply[R, T](
      resolver: Field => A => ZQuery[R, CalibanError, Option[T]]
    )(implicit schema: Schema[R, T], argBuilder: ArgBuilder[A]): EntityResolver[R] =
      new EntityResolver[R] {
        override def resolve(value: InputValue): Step[R] =
          QueryStep(ZQuery.fromEither(argBuilder.build(value)).map { arg =>
            Step.MetadataFunctionStep(field =>
              Step.QueryStep(resolver(field)(arg).map(_.fold[Step[R]](Step.NullStep)(schema.resolve)))
            )
          })

        override def toType: __Type = schema.toType_()
      }
  }

  class EntityResolverPartiallyApplied[A](val dummy: Boolean = false) {
    def apply[R, R1 <: R, T](
      resolver: A => ZQuery[R1, CalibanError, Option[T]]
    )(implicit schema: Schema[R, T], argBuilder: ArgBuilder[A]): EntityResolver[R1] =
      EntityResolver[R1, A, T](resolver)
  }
}
