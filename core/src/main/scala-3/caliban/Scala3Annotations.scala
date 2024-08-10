package caliban

import scala.annotation

/**
 * Proxies for annotations that exist in Scala 3 but not in Scala 2
 */
private[caliban] object Scala3Annotations {
  type transparentTrait = annotation.transparentTrait
}
