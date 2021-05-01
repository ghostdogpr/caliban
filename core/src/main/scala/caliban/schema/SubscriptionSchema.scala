package caliban.schema

import zio.stream.ZStream

/**
 * Typeclass used to guarantee that the Subscriptions type is either `Unit` or a case class with `zio.stream.ZStream` for fields.
 */
trait SubscriptionSchema[T]

object SubscriptionSchema extends SubscriptionSchemaDerivation {

  implicit val unitSubscriptionSchema: SubscriptionSchema[Unit]                                      = new SubscriptionSchema[Unit] {}
  implicit def streamSubscriptionSchema[R, E, A]: SubscriptionSchema[ZStream[R, E, A]]               =
    new SubscriptionSchema[ZStream[R, E, A]] {}
  implicit def functionSubscriptionSchema[R, E, A, ARG]: SubscriptionSchema[ARG => ZStream[R, E, A]] =
    new SubscriptionSchema[ARG => ZStream[R, E, A]] {}
}
