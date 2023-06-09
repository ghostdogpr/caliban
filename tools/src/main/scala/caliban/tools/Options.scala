package caliban.tools

import zio.{ UIO, ZIO }
import zio.config.magnolia.Descriptor
import zio.config.{ read, ConfigDescriptor, ConfigSource }

final case class Options(
  schemaPath: String,
  toPath: String,
  fmtPath: Option[String],
  headers: Option[List[Options.Header]],
  packageName: Option[String],
  clientName: Option[String],
  genView: Option[Boolean],
  effect: Option[String],
  scalarMappings: Option[Map[String, String]],
  imports: Option[List[String]],
  abstractEffectType: Option[Boolean],
  splitFiles: Option[Boolean],
  enableFmt: Option[Boolean],
  extensibleEnums: Option[Boolean],
  preserveInputNames: Option[Boolean],
  supportIsRepeatable: Option[Boolean],
  addDerives: Option[Boolean]
)

object Options {
  final case class Header(name: String, value: String)
  final case class RawOptions(
    scalafmtPath: Option[String],
    headers: Option[List[String]],
    packageName: Option[String],
    clientName: Option[String],
    genView: Option[Boolean],
    effect: Option[String],
    scalarMappings: Option[List[String]],
    imports: Option[List[String]],
    abstractEffectType: Option[Boolean],
    splitFiles: Option[Boolean],
    enableFmt: Option[Boolean],
    extensibleEnums: Option[Boolean],
    preserveInputNames: Option[Boolean],
    supportIsRepeatable: Option[Boolean],
    addDerives: Option[Boolean]
  )

  def fromArgs(args: List[String]): UIO[Option[Options]] =
    args match {
      case schemaPath :: toPath :: other =>
        val configSource: ConfigSource                     =
          ConfigSource.fromCommandLineArgs(
            args = other,
            keyDelimiter = Some('.'),
            valueDelimiter = Some(',')
          )
        val configDescriptor: ConfigDescriptor[RawOptions] = DescriptorUtils.from[RawOptions](configSource)

        read(configDescriptor).map { rawOpts =>
          Options(
            schemaPath,
            toPath,
            rawOpts.scalafmtPath,
            rawOpts.headers.map {
              _.flatMap { rawHeader =>
                rawHeader.split(":").toList match {
                  case name :: values if values.nonEmpty => Some(Header(name, values.mkString(":")))
                  case _                                 => None
                }
              }
            },
            rawOpts.packageName,
            rawOpts.clientName,
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
            rawOpts.imports,
            rawOpts.abstractEffectType,
            rawOpts.splitFiles,
            rawOpts.enableFmt,
            rawOpts.extensibleEnums,
            rawOpts.preserveInputNames,
            rawOpts.supportIsRepeatable,
            rawOpts.addDerives
          )
        }.option
      case _                             => ZIO.none
    }
}
