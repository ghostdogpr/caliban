package caliban

import caliban.interop.play.IsPlayJsonWrites
import caliban.interop.zio.IsZIOJsonEncoder

private[caliban] trait CalibanErrorJsonCompat {
  implicit def playJsonWrites[F[_]](implicit ev: IsPlayJsonWrites[F]): F[CalibanError] =
    caliban.interop.play.json.ErrorPlayJson.errorValueWrites.asInstanceOf[F[CalibanError]]
  implicit def zioJsonEncoder[F[_]](implicit ev: IsZIOJsonEncoder[F]): F[CalibanError] =
    caliban.interop.zio.ErrorZioJson.errorValueEncoder.asInstanceOf[F[CalibanError]]
}
