package caliban.codegen

import caliban.tools.Options
import zio.config.magnolia.DeriveConfig
import zio.{ Config, ConfigProvider, UIO, ZIO }
import scala.annotation.tailrec

object OptionsParser {
  final private case class RawOptions(
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
    addDerives: Option[Boolean],
    envForDerives: Option[String],
    excludeDeprecated: Option[Boolean],
    supportDeprecatedArgs: Option[Boolean]
  )

  private object RawOptions {
    val config: Config[RawOptions] = DeriveConfig.deriveConfig[RawOptions]
  }

  def fromArgs(args: List[String]): UIO[Option[Options]] =
    args match {
      case schemaPath :: toPath :: other =>
        extractArgs(other) match {
          case Some(configProvider) =>
            configProvider
              .load(RawOptions.config)
              .map { rawOpts =>
                Options(
                  schemaPath,
                  toPath,
                  rawOpts.scalafmtPath,
                  rawOpts.headers.map {
                    _.flatMap { rawHeader =>
                      rawHeader.split(":").toList match {
                        case name :: values if values.nonEmpty => Some(Options.Header(name, values.mkString(":")))
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
                  rawOpts.addDerives,
                  rawOpts.envForDerives,
                  rawOpts.excludeDeprecated,
                  rawOpts.supportDeprecatedArgs
                )
              }
              .option
          case None                 => ZIO.none
        }
      case _                             => ZIO.none
    }

  private def extractArgs(args: List[String]): Option[ConfigProvider] = {
    val argMap = Map.newBuilder[String, String]

    @tailrec
    def loop(stack: List[String]): Option[Map[String, String]] =
      stack match {
        case key :: value :: rest =>
          if (key.startsWith("--")) {
            argMap += key.drop(2) -> value
            loop(rest)
          } else {
            None
          }
        case unmatched :: Nil     =>
          None
        case Nil                  =>
          Some(argMap.result())
      }

    loop(args).map(ConfigProvider.fromMap(_))
  }

}
