package caliban

import scala.annotation.static

import scala.collection.mutable

private[caliban] object syntax {
  @static val NullFn: () => AnyRef = () => null

  extension [K, V <: AnyRef](inline map: Map[K, V]) {
    transparent inline def getOrElseNull(key: K): V = map.getOrElse(key, NullFn()).asInstanceOf[V]
  }

  extension [K, V <: AnyRef](inline map: mutable.HashMap[K, V]) {
    transparent inline def getOrElseNull(key: K): V = map.getOrElse(key, NullFn()).asInstanceOf[V]
  }

  extension [A](inline list: List[A]) {

    /**
     * In Scala 3, foreach is not inlined which can lead to performance issues when used in hot paths.
     * In Scala 2 this method simply points to foreach, but in Scala 3 it is inlined to a while loop.
     */
    inline def foreachOne(inline f: A => Any): Unit = {
      var rem = list
      while (rem ne Nil) {
        f(rem.head)
        rem = rem.tail
      }
    }
  }
}

// Required for @static fields
private final class syntax private
