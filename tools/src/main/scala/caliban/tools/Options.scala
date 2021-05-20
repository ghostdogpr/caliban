package caliban.tools

import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.{ read, ConfigDescriptor, ConfigSource }

final case class Options(
  schemaPath: String,
  toPath: String,
  fmtPath: Option[String],
  headers: Option[List[Options.Header]],
  packageName: Option[String],
  genView: Option[Boolean],
  effect: Option[String],
  scalarMappings: Option[Map[String, String]],
  imports: Option[List[String]]
)

object Options {
  final case class Header(name: String, value: String)
  final case class RawOptions(
    scalafmtPath: Option[String],
    headers: Option[List[String]],
    packageName: Option[String],
    genView: Option[Boolean],
    effect: Option[String],
    scalarMappings: Option[List[String]],
    imports: Option[List[String]]
  )

  def fromArgs(args: List[String]): Option[Options] =
    args match {
      case schemaPath :: toPath :: other =>
        val configSource: ConfigSource                     =
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
            rawOpts.packageName,
            rawOpts.genView,
            rawOpts.effect,
            rawOpts.scalarMappings.map {
              _.flatMap { rawMapping =>
                rawMapping.split(":").toList match {
                  case name :: value :: Nil => Some(name -> value)
                  case _                    => None
                }
              }.toMap
            },
            rawOpts.imports
          )
        }
      case _                             => None
    }
}
