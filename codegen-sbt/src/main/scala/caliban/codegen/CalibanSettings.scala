package caliban.codegen

case class CalibanSettings(
  scalafmtPath: Option[String],
  headers: Seq[(String, String)],
  packageName: Option[String],
  genView: Option[Boolean],
  effect: Option[String],
  scalarMappings: Seq[(String, String)],
  imports: Seq[String]
) {
  def append(other: CalibanSettings): CalibanSettings =
    CalibanSettings(
      scalafmtPath = other.scalafmtPath.orElse(scalafmtPath),
      headers = headers ++ other.headers,
      packageName = other.packageName.orElse(packageName),
      genView = other.genView.orElse(genView),
      effect = other.effect.orElse(effect),
      scalarMappings = scalarMappings ++ other.scalarMappings,
      imports = imports ++ other.imports
    )

  def scalafmtPath(path: String): CalibanSettings                = this.copy(scalafmtPath = Some(path))
  def headers(pairs: (String, String)*): CalibanSettings         = this.copy(headers = this.headers ++ pairs)
  def packageName(name: String): CalibanSettings                 = this.copy(packageName = Some(name))
  def genView(value: Boolean): CalibanSettings                   = this.copy(genView = Some(value))
  def effect(tpe: String): CalibanSettings                       = this.copy(effect = Some(tpe))
  def scalarMapping(mapping: (String, String)*): CalibanSettings =
    this.copy(scalarMappings = this.scalarMappings ++ mapping)
  def imports(values: String*): CalibanSettings                  = this.copy(imports = this.imports ++ values)
}

object CalibanSettings {
  type Transformer = CalibanSettings => CalibanSettings
  val empty = CalibanSettings(
    scalafmtPath = Option.empty[String],
    headers = Seq.empty[(String, String)],
    packageName = Option.empty[String],
    genView = Option.empty[Boolean],
    effect = Option.empty[String],
    scalarMappings = Seq.empty[(String, String)],
    imports = Seq.empty[String]
  )
}
