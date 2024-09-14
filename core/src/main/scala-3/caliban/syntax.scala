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
}

// Required for @static fields
private final class syntax private
