package caliban

package object gateway {
  private[gateway] def traverseOption[A, B](values: Iterable[A])(f: A => Option[B]): Option[List[B]] = {
    val collected = List.newBuilder[B]
    val iterator  = values.iterator
    while (iterator.hasNext)
      f(iterator.next()) match {
        case Some(value) => collected += value
        case None        => return None
      }
    Some(collected.result())
  }
}
