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
        ArgBuilder[A].build(value) match {
          case Right(arg)  =>
            val q = resolver(arg).map {
              case Some(value) => schema.resolve(value)
              case _           => Step.NullStep
            }
            Step.QueryStep(q)
          case Left(error) => Step.FailureStep(error)
        }

      override def toType: __Type = schema.toType_()
    }

  def fromEither[A: ArgBuilder, T](
    resolver: A => Either[CalibanError, Option[T]]
  )(implicit schema: Schema[Any, T]): EntityResolver[Any] =
    new EntityResolver[Any] {
      override def resolve(value: InputValue): Step[Any] =
        ArgBuilder[A].build(value) match {
          case Right(arg)  =>
            val q = resolver(arg).map {
              case Some(value) => schema.resolve(value)
              case _           => Step.NullStep
            }
            Step.QueryStep(ZQuery.fromEither(q))
          case Left(error) => Step.FailureStep(error)
        }

      override def toType: __Type = schema.toType_()
    }

  def fromOption[A: ArgBuilder, T](
    resolver: A => Option[T]
  )(implicit schema: Schema[Any, T]): EntityResolver[Any] =
    new EntityResolver[Any] {
      override def resolve(value: InputValue): Step[Any] =
        ArgBuilder[A].build(value) match {
          case Right(arg)  =>
            resolver(arg) match {
              case Some(value) => schema.resolve(value)
              case _           => Step.NullStep
            }
          case Left(error) => Step.FailureStep(error)
        }

      override def toType: __Type = schema.toType_()
    }

  def fromZIO[R, A: ArgBuilder, T](
    resolver: A => ZIO[R, CalibanError, Option[T]]
  )(implicit schema: Schema[R, T]): EntityResolver[R] =
    new EntityResolver[R] {
      override def resolve(value: InputValue): Step[R] =
        ArgBuilder[A].build(value) match {
          case Right(arg)  =>
            val q = resolver(arg).map {
              case Some(value) => schema.resolve(value)
              case _           => Step.NullStep
            }
            Step.QueryStep(ZQuery.fromZIONow(q))
          case Left(error) => Step.FailureStep(error)
        }

      override def toType: __Type = schema.toType_()
    }

  def fromQuery[R, A: ArgBuilder, T](
    resolver: A => ZQuery[R, CalibanError, Option[T]]
  )(implicit schema: Schema[R, T]): EntityResolver[R] =
    apply(resolver)

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
          ArgBuilder[A].build(value) match {
            case Right(arg)  =>
              val q = (field: Field) =>
                resolver(field)(arg).map {
                  case Some(value) => schema.resolve(value)
                  case _           => Step.NullStep
                }
              Step.MetadataFunctionStep(field => Step.QueryStep(q(field)))
            case Left(error) => Step.FailureStep(error)
          }

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
