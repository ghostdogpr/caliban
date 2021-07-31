package caliban

package object codegen {
  @deprecated("CodegenPlugin has been renamed to CalibanPlugin", "1.1.0")
  val CodegenPlugin: CalibanPlugin.type = CalibanPlugin
}
