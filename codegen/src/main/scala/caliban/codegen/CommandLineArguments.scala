package caliban.codegen

import zio.config.{ read, ConfigDescriptor, ConfigSource }
import zio.config.magnolia.DeriveConfigDescriptor.descriptor

final case class CommandLineArguments(
  schemaPath: String,
  toPath: String,
  fmtPath: Option[String],
  headers: Option[List[CommandLineArguments.Header]]
)

object CommandLineArguments {
  final case class Header(name: String, value: String)
  final case class RawOptions(scalafmtPath: Option[String], headers: Option[List[String]])

  def fromArgs(args: List[String]): Option[CommandLineArguments] =
    args match {
      case schemaPath :: toPath :: other =>
        val configSource: ConfigSource[String, String] =
          ConfigSource.fromCommandLineArgs(
            args = other,
            keyDelimiter = Some('.'),
            valueDelimiter = Some(',')
          )
        val configDescriptor: ConfigDescriptor[String, String, RawOptions] = descriptor[RawOptions] from configSource

        read(configDescriptor).toOption.map { rawOpts =>
          CommandLineArguments(
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
            }
          )
        }
      case _ => None
    }
}
