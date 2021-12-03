package caliban

import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }
import caliban.interop.zio.{ IsZIOJsonDecoder, IsZIOJsonEncoder }

private[caliban] trait CalibanErrorJsonCompat {
  implicit def playJsonWrites[F[_]](implicit ev: IsPlayJsonWrites[F]): F[CalibanError] =
    caliban.interop.play.json.ErrorPlayJson.errorValueWrites.asInstanceOf[F[CalibanError]]
  implicit def playJsonReads[F[_]](implicit ev: IsPlayJsonReads[F]): F[CalibanError]   =
    caliban.interop.play.json.ErrorPlayJson.errorValueReads.asInstanceOf[F[CalibanError]]
//  implicit def zioJsonEncoder[F[_]](implicit ev: IsZIOJsonEncoder[F]): F[CalibanError] =
//    caliban.interop.zio.ErrorZioJson.errorValueEncoder.asInstanceOf[F[CalibanError]]
//  implicit def zioJsonDecoder[F[_]](implicit ev: IsZIOJsonEncoder[F]): F[CalibanError] =
//    caliban.interop.zio.ErrorZioJson.errorValueDecoder.asInstanceOf[F[CalibanError]]
}
