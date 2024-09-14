package caliban

import scala.annotation.StaticAnnotation

/**
 * Stubs for annotations that exist in Scala 3 but not in Scala 2
 */
private[caliban] object Scala3Annotations {
  final class static extends StaticAnnotation
}
