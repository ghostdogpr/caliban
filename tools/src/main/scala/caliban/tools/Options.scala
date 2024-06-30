package caliban.tools

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
  addDerives: Option[Boolean],
  envForDerives: Option[String],
  excludeDeprecated: Option[Boolean],
  queriesPath: Option[String]
)

object Options {
  final case class Header(name: String, value: String)
}
