package caliban

import caliban.interop.spray.{ IsSprayJsonReader, IsSprayJsonWriter }

private[caliban] trait ResponseValuePlatformSpecific {
  implicit def sprayJsonWriter[F[_]: IsSprayJsonWriter]: F[ResponseValue] =
    ValueSprayJson.responseValueWriter.asInstanceOf[F[ResponseValue]]
  implicit def sprayJsonReader[F[_]: IsSprayJsonReader]: F[ResponseValue] =
    ValueSprayJson.responseValueReader.asInstanceOf[F[ResponseValue]]
}

private[caliban] object ResponseValuePlatformSpecific extends ResponseValuePlatformSpecific
