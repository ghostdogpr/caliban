package caliban

import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }
import caliban.interop.zio.{ IsZIOJsonDecoder, IsZIOJsonEncoder }

private[caliban] trait ValueJsonCompat {
  implicit def inputValuePlayJsonWrites[F[_]: IsPlayJsonWrites]: F[InputValue] =
    caliban.interop.play.json.ValuePlayJson.inputValueWrites.asInstanceOf[F[InputValue]]
  implicit def inputValuePlayJsonReads[F[_]: IsPlayJsonReads]: F[InputValue]   =
    caliban.interop.play.json.ValuePlayJson.inputValueReads.asInstanceOf[F[InputValue]]

//  implicit def inputValueZioJsonEncoder[F[_]: IsZIOJsonEncoder]: F[InputValue] =
//    caliban.interop.zio.ValueZIOJson.inputValueEncoder.asInstanceOf[F[InputValue]]
//  implicit def inputValueZioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[InputValue] =
//    caliban.interop.zio.ValueZIOJson.inputValueDecoder.asInstanceOf[F[InputValue]]

  implicit def responseValuePlayJsonWrites[F[_]: IsPlayJsonWrites]: F[ResponseValue] =
    caliban.interop.play.json.ValuePlayJson.responseValueWrites.asInstanceOf[F[ResponseValue]]
  implicit def responseValuePlayJsonReads[F[_]: IsPlayJsonReads]: F[ResponseValue]   =
    caliban.interop.play.json.ValuePlayJson.responseValueReads.asInstanceOf[F[ResponseValue]]

//  implicit def responseValueZioJsonEncoder[F[_]: IsZIOJsonEncoder]: F[ResponseValue] =
//    caliban.interop.zio.ValueZIOJson.responseValueEncoder.asInstanceOf[F[ResponseValue]]
//  implicit def responseValueZioJsonDecoder[F[_]: IsZIOJsonDecoder]: F[ResponseValue] =
//    caliban.interop.zio.ValueZIOJson.responseValueDecoder.asInstanceOf[F[ResponseValue]]
}
