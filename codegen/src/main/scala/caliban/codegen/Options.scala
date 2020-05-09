package caliban.codegen

import zio.config.{ read, ConfigDescriptor, ConfigSource }
import zio.config.magnolia.DeriveConfigDescriptor.descriptor

final case class Options(
  schemaPath: String,
  toPath: String,
  fmtPath: Option[String],
  headers: Option[List[Options.Header]],
  packageName: Option[String]
)

object Options {
  final case class Header(name: String, value: String)
  final case class RawOptions(scalafmtPath: Option[String], headers: Option[List[String]], packageName: Option[String])

  def fromArgs(args: List[String]): Option[Options] =
    args match {
      case schemaPath :: toPath :: other =>
        val configSource: ConfigSource =
          ConfigSource.fromCommandLineArgs(
            args = other,
            keyDelimiter = Some('.'),
            valueDelimiter = Some(',')
          )
        val configDescriptor: ConfigDescriptor[RawOptions] = descriptor[RawOptions] from configSource

        read(configDescriptor).toOption.map { rawOpts =>
          Options(
            schemaPath,
            toPath,
            rawOpts.scalafmtPath,
            rawOpts.headers.map {
              _.flatMap { rawHeader =>
                rawHeader.split(":").toList match {
                  case name :: value :: Nil => Some(Header(name, value))
                  case _                    => None
                }
              }
            },
            rawOpts.packageName
          )
        }
      case _ => None
    }
}
