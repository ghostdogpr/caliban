package caliban

import zio.Unsafe

private object implicits {
  implicit val unsafe: Unsafe = Unsafe.unsafe(identity)
}
