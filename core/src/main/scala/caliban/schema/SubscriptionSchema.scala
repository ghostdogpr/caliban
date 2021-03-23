package caliban.schema

import scala.language.experimental.macros

import magnolia._
import zio.stream.ZStream

/**
 * Typeclass used to guarantee that the Subscriptions type is either `Unit` or a case class with `zio.stream.ZStream` for fields.
 */
trait SubscriptionSchema[-T]

object SubscriptionSchema {

  type Typeclass[T] = SubscriptionSchema[T]

  implicit val unitSubscriptionSchema: Typeclass[Unit]                                      = new Typeclass[Unit] {}
  implicit def streamSubscriptionSchema[R, E, A]: Typeclass[ZStream[R, E, A]]               = new Typeclass[ZStream[R, E, A]] {}
  implicit def functionSubscriptionSchema[R, E, A, ARG]: Typeclass[ARG => ZStream[R, E, A]] =
    new Typeclass[ARG => ZStream[R, E, A]] {}

  def combine[T](ctx: CaseClass[SubscriptionSchema, T]): Typeclass[T]                       = new Typeclass[T] {}

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
