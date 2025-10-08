package caliban

import scala.util.hashing.MurmurHash3

object Hash {
  def caseClassHash(x: Product): Int = MurmurHash3.productHash(x)
}
