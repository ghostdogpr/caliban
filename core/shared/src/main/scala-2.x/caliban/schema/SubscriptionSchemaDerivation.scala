package caliban.schema

import magnolia._

import scala.language.experimental.macros

trait SubscriptionSchemaDerivation {
  type Typeclass[T] = SubscriptionSchema[T]

  def combine[T](ctx: CaseClass[SubscriptionSchema, T]): Typeclass[T] = new Typeclass[T] {}

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}
