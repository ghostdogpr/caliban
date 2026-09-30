package caliban.gateway.internal

import scala.annotation.tailrec

private[gateway] final class PrivateAliases(used: Set[String]) {
  private var names = used

  def next(base: String): String = {
    @tailrec
    def find(candidate: String, suffix: Int): String =
      if (names.contains(candidate)) find(s"${base}_$suffix", suffix + 1)
      else candidate

    val alias = find(base, 2)
    names += alias
    alias
  }
}
