package caliban

import caliban.interop.spray.{ IsSprayJsonReader, IsSprayJsonWriter }

private[caliban] trait InputValuePlatformSpecific {
  implicit def sprayJsonWriter[F[_]: IsSprayJsonWriter]: F[InputValue] =
    ValueSprayJson.inputValueWriter.asInstanceOf[F[InputValue]]
  implicit def sprayJsonReader[F[_]: IsSprayJsonReader]: F[InputValue] =
    ValueSprayJson.inputValueReader.asInstanceOf[F[InputValue]]
}

private[caliban] object InputValuePlatformSpecific extends InputValuePlatformSpecific
