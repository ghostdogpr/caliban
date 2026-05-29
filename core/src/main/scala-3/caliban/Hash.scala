package caliban

import scala.annotation.nowarn
import scala.util.hashing.MurmurHash3

object Hash {
  @nowarn("cat=deprecation")
  def caseClassHash(x: Product): Int = MurmurHash3.productHash(x)
}
