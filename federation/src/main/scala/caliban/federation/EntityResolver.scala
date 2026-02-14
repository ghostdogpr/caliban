package caliban.federation

import caliban.CalibanError.ExecutionError
import caliban.execution.Field
import caliban.federation.v2x.FederationDirectivesV2_12.{ buildCacheTags, Cacheable }
import caliban.introspection.adt.__Type
import caliban.schema.Step.QueryStep
import caliban.schema.{ ArgBuilder, Extended, Schema, Step }
import caliban.{ CalibanError, InputValue, ResponseValue }
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

  def fromCachedZIO[R, A: ArgBuilder, T](
    resolver: A => ZIO[R, CalibanError, (Option[T], List[String])]
  )(implicit schema: Schema[R, T], cacheable: Cacheable): EntityResolver[R] =
    fromZIO[R, A, Extended[T]](resolver(_).map(fromCached))

  def fromCachedEither[A: ArgBuilder, T](
    resolver: A => Either[CalibanError, (Option[T], List[String])]
  )(implicit schema: Schema[Any, T], cacheable: Cacheable): EntityResolver[Any] =
    fromEither[A, Extended[T]](resolver(_).map(fromCached))

  def fromCachedQuery[R, A: ArgBuilder, T](
    resolver: A => ZQuery[R, CalibanError, (Option[T], List[String])]
  )(implicit schema: Schema[R, T], cacheable: Cacheable): EntityResolver[R] =
    fromQuery[R, A, Extended[T]](resolver(_).map(fromCached))

  def fromCachedOption[A: ArgBuilder, T](
    resolver: A => (Option[T], List[String])
  )(implicit schema: Schema[Any, T], cacheable: Cacheable): EntityResolver[Any] =
    fromOption[A, Extended[T]](a => fromCached(resolver(a)))

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

  private def fromCached[T](
    value: (Option[T], List[String])
  )(implicit cacheable: Cacheable): Option[Extended[T]] =
    value match {
      case (Some(value), Nil)  => Some(Extended(value, ResponseValue.ObjectValue.empty))
      case (Some(value), tags) => Some(Extended(value, cacheable.fromTags(tags)))
      case _                   => None
    }

  class EntityResolverPartiallyApplied[A](val dummy: Boolean = false) {
    def apply[R, R1 <: R, T](
      resolver: A => ZQuery[R1, CalibanError, Option[T]]
    )(implicit schema: Schema[R, T], argBuilder: ArgBuilder[A]): EntityResolver[R1] =
      EntityResolver[R1, A, T](resolver)
  }
}
