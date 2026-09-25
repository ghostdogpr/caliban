package caliban.gateway.internal

import scala.annotation.tailrec

private[gateway] final class PrivateAliases(used: Set[String]) {
  private var names = used

  def next(base: String): String = {
    val alias = PrivateAliases.privateAlias(base, names)
    names += alias
    alias
  }
}

private[gateway] object PrivateAliases {
  def privateAlias(base: String, used: Set[String]): String = {
    @tailrec
    def find(candidate: String, suffix: Int): String =
      if (used.contains(candidate)) find(s"${base}_$suffix", suffix + 1)
      else candidate

    find(base, 2)
  }
}
