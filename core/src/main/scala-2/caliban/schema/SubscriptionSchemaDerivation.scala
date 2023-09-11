package caliban.schema

import magnolia1._

import scala.language.experimental.macros

trait SubscriptionSchemaDerivation {
  type Typeclass[T] = SubscriptionSchema[T]

  def join[T](ctx: CaseClass[SubscriptionSchema, T]): Typeclass[T] = new Typeclass[T] {}

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}
