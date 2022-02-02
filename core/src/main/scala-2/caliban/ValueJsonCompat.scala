package caliban

import caliban.interop.play.{ IsPlayJsonReads, IsPlayJsonWrites }

private[caliban] trait ValueJsonCompat {
  implicit def inputValuePlayJsonWrites[F[_]: IsPlayJsonWrites]: F[InputValue]       =
    caliban.interop.play.json.ValuePlayJson.inputValueWrites.asInstanceOf[F[InputValue]]
  implicit def inputValuePlayJsonReads[F[_]: IsPlayJsonReads]: F[InputValue]         =
    caliban.interop.play.json.ValuePlayJson.inputValueReads.asInstanceOf[F[InputValue]]
  implicit def responseValuePlayJsonWrites[F[_]: IsPlayJsonWrites]: F[ResponseValue] =
    caliban.interop.play.json.ValuePlayJson.responseValueWrites.asInstanceOf[F[ResponseValue]]
  implicit def responseValuePlayJsonReads[F[_]: IsPlayJsonReads]: F[ResponseValue]   =
    caliban.interop.play.json.ValuePlayJson.responseValueReads.asInstanceOf[F[ResponseValue]]
}
