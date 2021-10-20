package caliban.schema

import scala.deriving.Mirror
import scala.compiletime._

trait SubscriptionSchemaDerivation {
  inline def checkParams[T <: Tuple]: Unit =
    inline erasedValue[T] match {
      case _: EmptyTuple => ()
      case _: (t *: ts)  =>
        summonInline[SubscriptionSchema[t]]
        checkParams[ts]
    }

  inline def derived[A]: SubscriptionSchema[A] =
    inline summonInline[Mirror.ProductOf[A]] match {
      case m: Mirror.ProductOf[A] =>
        checkParams[m.MirroredElemTypes]
        new SubscriptionSchema[A] {}
    }

  inline given gen[A]: SubscriptionSchema[A] = derived
}
