package caliban.codegen

sealed trait CalibanSettings {
  def clientName: Option[String]
  def scalafmtPath: Option[String]
  def headers: Seq[(String, String)]
  def packageName: Option[String]
  def genView: Option[Boolean]
  def effect: Option[String]
  def scalarMappings: Seq[(String, String)]
  def imports: Seq[String]
}

case class CalibanFileSettings(
  clientName: Option[String],
  scalafmtPath: Option[String],
  packageName: Option[String],
  genView: Option[Boolean],
  effect: Option[String],
  scalarMappings: Seq[(String, String)],
  imports: Seq[String]
) extends CalibanSettings {
  val headers = Seq.empty // Not applicable for file generator

  def append(other: CalibanFileSettings): CalibanFileSettings =
    CalibanFileSettings(
      clientName = other.clientName.orElse(clientName),
      scalafmtPath = other.scalafmtPath.orElse(scalafmtPath),
      packageName = other.packageName.orElse(packageName),
      genView = other.genView.orElse(genView),
      effect = other.effect.orElse(effect),
      scalarMappings = scalarMappings ++ other.scalarMappings,
      imports = imports ++ other.imports
    )

  def clientName(value: String): CalibanFileSettings                 = this.copy(clientName = Some(value))
  def scalafmtPath(path: String): CalibanFileSettings                = this.copy(scalafmtPath = Some(path))
  def packageName(name: String): CalibanFileSettings                 = this.copy(packageName = Some(name))
  def genView(value: Boolean): CalibanFileSettings                   = this.copy(genView = Some(value))
  def effect(tpe: String): CalibanFileSettings                       = this.copy(effect = Some(tpe))
  def scalarMapping(mapping: (String, String)*): CalibanFileSettings =
    this.copy(scalarMappings = this.scalarMappings ++ mapping)
  def imports(values: String*): CalibanFileSettings                  = this.copy(imports = this.imports ++ values)
}

object CalibanSettings {
  type Transformer = CalibanSettings => CalibanSettings
  val empty = CalibanFileSettings(
    clientName = Option.empty[String],
    scalafmtPath = Option.empty[String],
    packageName = Option.empty[String],
    genView = Option.empty[Boolean],
    effect = Option.empty[String],
    scalarMappings = Seq.empty[(String, String)],
    imports = Seq.empty[String]
  )
}
