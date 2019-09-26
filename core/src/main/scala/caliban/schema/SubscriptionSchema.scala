package caliban.schema

import scala.language.experimental.macros
import magnolia._
import zio.stream.ZStream

trait SubscriptionSchema[T]

object SubscriptionSchema {

  type Typeclass[T] = SubscriptionSchema[T]

  implicit val unitSubscriptionSchema: Typeclass[Unit]                        = new Typeclass[Unit]             {}
  implicit def streamSubscriptionSchema[R, E, A]: Typeclass[ZStream[R, E, A]] = new Typeclass[ZStream[R, E, A]] {}

  def combine[T](ctx: CaseClass[SubscriptionSchema, T]): Typeclass[T] = new Typeclass[T] {}

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]

}
