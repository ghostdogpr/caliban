package caliban.tools

import caliban.GraphQL
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.{ read, ConfigDescriptor, ConfigSource }

sealed trait GenSource
object GenSource {
  case object Url                              extends GenSource
  case object File                             extends GenSource
  final case class Caliban[R](api: GraphQL[R]) extends GenSource
}

final case class Options(
  source: GenSource,
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
  extensibleEnums: Option[Boolean]
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
    extensibleEnums: Option[Boolean]
  )

  private def fromUrlOrFile(
    source: GenSource,
    schemaPath: String,
    toPath: String,
    other: List[String]
  ): Option[Options] = {
    val configSource: ConfigSource =
      ConfigSource.fromCommandLineArgs(
        args = other,
        keyDelimiter = Some('.'),
        valueDelimiter = Some(',')
      )

    val configDescriptor: ConfigDescriptor[RawOptions] = descriptor[RawOptions] from configSource

    read(configDescriptor).toOption.map { rawOpts =>
      Options(
        source,
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
        rawOpts.extensibleEnums
      )
    }
  }

  def fromArgs(args: List[String]): Option[Options] =
    args match {
      case "caliban" :: codePath :: toPath :: other => ??? // TODO Jules
      case "url" :: schemaPath :: toPath :: other   => fromUrlOrFile(GenSource.Url, schemaPath, toPath, other)
      case "file" :: schemaPath :: toPath :: other  => fromUrlOrFile(GenSource.File, schemaPath, toPath, other)
      case _                                        => None
    }
}
