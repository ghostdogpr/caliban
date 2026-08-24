package caliban.gateway

/**
 * An immutable structural change applied to one subgraph before gateway composition.
 */
sealed trait SchemaTransformation

/**
 * Constructors for gateway schema transformations.
 *
 * Gateway `hide*` transformations correspond to the `Exclude*` terminology used by core schema transformers such as
 * `caliban.transformers.Transformer.ExcludeField`, `caliban.transformers.Transformer.ExcludeInputField`, and
 * `caliban.transformers.Transformer.ExcludeArgument`. Gateway transformations additionally rewrite downstream
 * operations and responses so hidden or renamed source coordinates remain routable.
 */
object SchemaTransformation {

  /**
   * Renames a non-operation type.
   */
  def renameType(name: String, renamed: String): SchemaTransformation =
    RenameType(name, renamed)

  /**
   * Hides a type from the composed client schema.
   */
  def hideType(name: String): SchemaTransformation =
    HideType(name)

  /**
   * Renames a field on an object or interface.
   */
  def renameField(typeName: String, name: String, renamed: String): SchemaTransformation =
    RenameField(typeName, name, renamed)

  /**
   * Hides a field from the composed client schema.
   */
  def hideField(typeName: String, name: String): SchemaTransformation =
    HideField(typeName, name)

  /**
   * Renames a field argument.
   */
  def renameArgument(typeName: String, field: String, name: String, renamed: String): SchemaTransformation =
    RenameArgument(typeName, field, name, renamed)

  /**
   * Hides an optional field argument from the composed client schema.
   */
  def hideArgument(typeName: String, field: String, name: String): SchemaTransformation =
    HideArgument(typeName, field, name)

  /**
   * Renames an input-object field.
   */
  def renameInputField(typeName: String, name: String, renamed: String): SchemaTransformation =
    RenameInputField(typeName, name, renamed)

  /**
   * Hides an optional input-object field from the composed client schema.
   */
  def hideInputField(typeName: String, name: String): SchemaTransformation =
    HideInputField(typeName, name)

  /**
   * Renames an enum value.
   */
  def renameEnumValue(typeName: String, value: String, renamed: String): SchemaTransformation =
    RenameEnumValue(typeName, value, renamed)

  /**
   * Hides an enum value from the composed client schema.
   */
  def hideEnumValue(typeName: String, value: String): SchemaTransformation =
    HideEnumValue(typeName, value)

  private[gateway] final case class RenameType(name: String, renamed: String) extends SchemaTransformation
  private[gateway] final case class HideType(name: String)                    extends SchemaTransformation

  private[gateway] final case class RenameField(typeName: String, name: String, renamed: String)
      extends SchemaTransformation
  private[gateway] final case class HideField(typeName: String, name: String) extends SchemaTransformation

  private[gateway] final case class RenameArgument(typeName: String, field: String, name: String, renamed: String)
      extends SchemaTransformation
  private[gateway] final case class HideArgument(typeName: String, field: String, name: String)
      extends SchemaTransformation

  private[gateway] final case class RenameInputField(typeName: String, name: String, renamed: String)
      extends SchemaTransformation
  private[gateway] final case class HideInputField(typeName: String, name: String) extends SchemaTransformation

  private[gateway] final case class RenameEnumValue(typeName: String, value: String, renamed: String)
      extends SchemaTransformation
  private[gateway] final case class HideEnumValue(typeName: String, value: String) extends SchemaTransformation
}
