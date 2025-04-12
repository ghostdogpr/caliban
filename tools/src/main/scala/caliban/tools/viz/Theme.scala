package caliban.tools.viz

case class Theme(
  color: String,
  align: String
)

object Theme {
  def default = Theme(color = "black", align = "LEFT")
}
