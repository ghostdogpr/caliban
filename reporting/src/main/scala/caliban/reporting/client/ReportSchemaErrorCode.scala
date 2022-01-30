package caliban.reporting.client

import caliban.client.CalibanClientError.DecodingError
import caliban.client._
import caliban.client.__Value._

sealed trait ReportSchemaErrorCode extends scala.Product with scala.Serializable { def value: String }
object ReportSchemaErrorCode {
  case object BOOT_ID_IS_NOT_VALID_UUID                 extends ReportSchemaErrorCode {
    val value: String = "BOOT_ID_IS_NOT_VALID_UUID"
  }
  case object BOOT_ID_IS_REQUIRED                       extends ReportSchemaErrorCode { val value: String = "BOOT_ID_IS_REQUIRED"      }
  case object CORE_SCHEMA_HASH_IS_NOT_SCHEMA_SHA256     extends ReportSchemaErrorCode {
    val value: String = "CORE_SCHEMA_HASH_IS_NOT_SCHEMA_SHA256"
  }
  case object CORE_SCHEMA_HASH_IS_REQUIRED              extends ReportSchemaErrorCode {
    val value: String = "CORE_SCHEMA_HASH_IS_REQUIRED"
  }
  case object CORE_SCHEMA_HASH_IS_TOO_LONG              extends ReportSchemaErrorCode {
    val value: String = "CORE_SCHEMA_HASH_IS_TOO_LONG"
  }
  case object EXECUTABLE_SCHEMA_ID_IS_NOT_SCHEMA_SHA256 extends ReportSchemaErrorCode {
    val value: String = "EXECUTABLE_SCHEMA_ID_IS_NOT_SCHEMA_SHA256"
  }
  case object EXECUTABLE_SCHEMA_ID_IS_REQUIRED          extends ReportSchemaErrorCode {
    val value: String = "EXECUTABLE_SCHEMA_ID_IS_REQUIRED"
  }
  case object EXECUTABLE_SCHEMA_ID_IS_TOO_LONG          extends ReportSchemaErrorCode {
    val value: String = "EXECUTABLE_SCHEMA_ID_IS_TOO_LONG"
  }
  case object GRAPH_REF_INVALID_FORMAT                  extends ReportSchemaErrorCode { val value: String = "GRAPH_REF_INVALID_FORMAT" }
  case object GRAPH_REF_IS_REQUIRED                     extends ReportSchemaErrorCode { val value: String = "GRAPH_REF_IS_REQUIRED"    }
  case object GRAPH_VARIANT_DOES_NOT_MATCH_REGEX        extends ReportSchemaErrorCode {
    val value: String = "GRAPH_VARIANT_DOES_NOT_MATCH_REGEX"
  }
  case object GRAPH_VARIANT_IS_REQUIRED                 extends ReportSchemaErrorCode {
    val value: String = "GRAPH_VARIANT_IS_REQUIRED"
  }
  case object LIBRARY_VERSION_IS_TOO_LONG               extends ReportSchemaErrorCode {
    val value: String = "LIBRARY_VERSION_IS_TOO_LONG"
  }
  case object PLATFORM_IS_TOO_LONG                      extends ReportSchemaErrorCode { val value: String = "PLATFORM_IS_TOO_LONG"     }
  case object RUNTIME_VERSION_IS_TOO_LONG               extends ReportSchemaErrorCode {
    val value: String = "RUNTIME_VERSION_IS_TOO_LONG"
  }
  case object SCHEMA_IS_NOT_PARSABLE                    extends ReportSchemaErrorCode { val value: String = "SCHEMA_IS_NOT_PARSABLE"   }
  case object SCHEMA_IS_NOT_VALID                       extends ReportSchemaErrorCode { val value: String = "SCHEMA_IS_NOT_VALID"      }
  case object SERVER_ID_IS_TOO_LONG                     extends ReportSchemaErrorCode { val value: String = "SERVER_ID_IS_TOO_LONG"    }
  case object USER_VERSION_IS_TOO_LONG                  extends ReportSchemaErrorCode { val value: String = "USER_VERSION_IS_TOO_LONG" }

  implicit val decoder: ScalarDecoder[ReportSchemaErrorCode] = {
    case __StringValue("BOOT_ID_IS_NOT_VALID_UUID")                 => Right(ReportSchemaErrorCode.BOOT_ID_IS_NOT_VALID_UUID)
    case __StringValue("BOOT_ID_IS_REQUIRED")                       => Right(ReportSchemaErrorCode.BOOT_ID_IS_REQUIRED)
    case __StringValue("CORE_SCHEMA_HASH_IS_NOT_SCHEMA_SHA256")     =>
      Right(ReportSchemaErrorCode.CORE_SCHEMA_HASH_IS_NOT_SCHEMA_SHA256)
    case __StringValue("CORE_SCHEMA_HASH_IS_REQUIRED")              => Right(ReportSchemaErrorCode.CORE_SCHEMA_HASH_IS_REQUIRED)
    case __StringValue("CORE_SCHEMA_HASH_IS_TOO_LONG")              => Right(ReportSchemaErrorCode.CORE_SCHEMA_HASH_IS_TOO_LONG)
    case __StringValue("EXECUTABLE_SCHEMA_ID_IS_NOT_SCHEMA_SHA256") =>
      Right(ReportSchemaErrorCode.EXECUTABLE_SCHEMA_ID_IS_NOT_SCHEMA_SHA256)
    case __StringValue("EXECUTABLE_SCHEMA_ID_IS_REQUIRED")          =>
      Right(ReportSchemaErrorCode.EXECUTABLE_SCHEMA_ID_IS_REQUIRED)
    case __StringValue("EXECUTABLE_SCHEMA_ID_IS_TOO_LONG")          =>
      Right(ReportSchemaErrorCode.EXECUTABLE_SCHEMA_ID_IS_TOO_LONG)
    case __StringValue("GRAPH_REF_INVALID_FORMAT")                  => Right(ReportSchemaErrorCode.GRAPH_REF_INVALID_FORMAT)
    case __StringValue("GRAPH_REF_IS_REQUIRED")                     => Right(ReportSchemaErrorCode.GRAPH_REF_IS_REQUIRED)
    case __StringValue("GRAPH_VARIANT_DOES_NOT_MATCH_REGEX")        =>
      Right(ReportSchemaErrorCode.GRAPH_VARIANT_DOES_NOT_MATCH_REGEX)
    case __StringValue("GRAPH_VARIANT_IS_REQUIRED")                 => Right(ReportSchemaErrorCode.GRAPH_VARIANT_IS_REQUIRED)
    case __StringValue("LIBRARY_VERSION_IS_TOO_LONG")               => Right(ReportSchemaErrorCode.LIBRARY_VERSION_IS_TOO_LONG)
    case __StringValue("PLATFORM_IS_TOO_LONG")                      => Right(ReportSchemaErrorCode.PLATFORM_IS_TOO_LONG)
    case __StringValue("RUNTIME_VERSION_IS_TOO_LONG")               => Right(ReportSchemaErrorCode.RUNTIME_VERSION_IS_TOO_LONG)
    case __StringValue("SCHEMA_IS_NOT_PARSABLE")                    => Right(ReportSchemaErrorCode.SCHEMA_IS_NOT_PARSABLE)
    case __StringValue("SCHEMA_IS_NOT_VALID")                       => Right(ReportSchemaErrorCode.SCHEMA_IS_NOT_VALID)
    case __StringValue("SERVER_ID_IS_TOO_LONG")                     => Right(ReportSchemaErrorCode.SERVER_ID_IS_TOO_LONG)
    case __StringValue("USER_VERSION_IS_TOO_LONG")                  => Right(ReportSchemaErrorCode.USER_VERSION_IS_TOO_LONG)
    case other                                                      => Left(DecodingError(s"Can't build ReportSchemaErrorCode from input $other"))
  }
  implicit val encoder: ArgEncoder[ReportSchemaErrorCode]    = {
    case ReportSchemaErrorCode.BOOT_ID_IS_NOT_VALID_UUID                 => __EnumValue("BOOT_ID_IS_NOT_VALID_UUID")
    case ReportSchemaErrorCode.BOOT_ID_IS_REQUIRED                       => __EnumValue("BOOT_ID_IS_REQUIRED")
    case ReportSchemaErrorCode.CORE_SCHEMA_HASH_IS_NOT_SCHEMA_SHA256     =>
      __EnumValue("CORE_SCHEMA_HASH_IS_NOT_SCHEMA_SHA256")
    case ReportSchemaErrorCode.CORE_SCHEMA_HASH_IS_REQUIRED              => __EnumValue("CORE_SCHEMA_HASH_IS_REQUIRED")
    case ReportSchemaErrorCode.CORE_SCHEMA_HASH_IS_TOO_LONG              => __EnumValue("CORE_SCHEMA_HASH_IS_TOO_LONG")
    case ReportSchemaErrorCode.EXECUTABLE_SCHEMA_ID_IS_NOT_SCHEMA_SHA256 =>
      __EnumValue("EXECUTABLE_SCHEMA_ID_IS_NOT_SCHEMA_SHA256")
    case ReportSchemaErrorCode.EXECUTABLE_SCHEMA_ID_IS_REQUIRED          => __EnumValue("EXECUTABLE_SCHEMA_ID_IS_REQUIRED")
    case ReportSchemaErrorCode.EXECUTABLE_SCHEMA_ID_IS_TOO_LONG          => __EnumValue("EXECUTABLE_SCHEMA_ID_IS_TOO_LONG")
    case ReportSchemaErrorCode.GRAPH_REF_INVALID_FORMAT                  => __EnumValue("GRAPH_REF_INVALID_FORMAT")
    case ReportSchemaErrorCode.GRAPH_REF_IS_REQUIRED                     => __EnumValue("GRAPH_REF_IS_REQUIRED")
    case ReportSchemaErrorCode.GRAPH_VARIANT_DOES_NOT_MATCH_REGEX        => __EnumValue("GRAPH_VARIANT_DOES_NOT_MATCH_REGEX")
    case ReportSchemaErrorCode.GRAPH_VARIANT_IS_REQUIRED                 => __EnumValue("GRAPH_VARIANT_IS_REQUIRED")
    case ReportSchemaErrorCode.LIBRARY_VERSION_IS_TOO_LONG               => __EnumValue("LIBRARY_VERSION_IS_TOO_LONG")
    case ReportSchemaErrorCode.PLATFORM_IS_TOO_LONG                      => __EnumValue("PLATFORM_IS_TOO_LONG")
    case ReportSchemaErrorCode.RUNTIME_VERSION_IS_TOO_LONG               => __EnumValue("RUNTIME_VERSION_IS_TOO_LONG")
    case ReportSchemaErrorCode.SCHEMA_IS_NOT_PARSABLE                    => __EnumValue("SCHEMA_IS_NOT_PARSABLE")
    case ReportSchemaErrorCode.SCHEMA_IS_NOT_VALID                       => __EnumValue("SCHEMA_IS_NOT_VALID")
    case ReportSchemaErrorCode.SERVER_ID_IS_TOO_LONG                     => __EnumValue("SERVER_ID_IS_TOO_LONG")
    case ReportSchemaErrorCode.USER_VERSION_IS_TOO_LONG                  => __EnumValue("USER_VERSION_IS_TOO_LONG")
  }

  val values: scala.collection.immutable.Vector[ReportSchemaErrorCode] = scala.collection.immutable.Vector(
    BOOT_ID_IS_NOT_VALID_UUID,
    BOOT_ID_IS_REQUIRED,
    CORE_SCHEMA_HASH_IS_NOT_SCHEMA_SHA256,
    CORE_SCHEMA_HASH_IS_REQUIRED,
    CORE_SCHEMA_HASH_IS_TOO_LONG,
    EXECUTABLE_SCHEMA_ID_IS_NOT_SCHEMA_SHA256,
    EXECUTABLE_SCHEMA_ID_IS_REQUIRED,
    EXECUTABLE_SCHEMA_ID_IS_TOO_LONG,
    GRAPH_REF_INVALID_FORMAT,
    GRAPH_REF_IS_REQUIRED,
    GRAPH_VARIANT_DOES_NOT_MATCH_REGEX,
    GRAPH_VARIANT_IS_REQUIRED,
    LIBRARY_VERSION_IS_TOO_LONG,
    PLATFORM_IS_TOO_LONG,
    RUNTIME_VERSION_IS_TOO_LONG,
    SCHEMA_IS_NOT_PARSABLE,
    SCHEMA_IS_NOT_VALID,
    SERVER_ID_IS_TOO_LONG,
    USER_VERSION_IS_TOO_LONG
  )
}

