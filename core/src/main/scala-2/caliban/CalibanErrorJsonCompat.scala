package caliban

import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }

private[caliban] trait CalibanErrorJsonCompat {
  implicit def playJsonWrites[F[_]](implicit ev: IsPlayJsonWrites[F]): F[CalibanError] =
    caliban.interop.play.json.ErrorPlayJson.errorValueWrites.asInstanceOf[F[CalibanError]]
  implicit def playJsonReads[F[_]](implicit ev: IsPlayJsonReads[F]): F[CalibanError]   =
    caliban.interop.play.json.ErrorPlayJson.errorValueReads.asInstanceOf[F[CalibanError]]
}
