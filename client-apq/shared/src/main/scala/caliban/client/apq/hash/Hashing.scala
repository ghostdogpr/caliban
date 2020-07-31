package caliban.client.apq.hash

import cats.data.Kleisli

private[apq] object Hashing {
  type Result[T]    = Either[String, T]
  type Hasher[A, B] = Kleisli[Result, A, B]
}
