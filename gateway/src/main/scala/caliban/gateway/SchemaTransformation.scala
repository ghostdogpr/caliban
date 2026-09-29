package caliban.gateway

import caliban.gateway.internal.composition.SchemaMapping._

/**
 * An immutable structural change applied to one subgraph before gateway composition.
 */
final class SchemaTransformation private (
  private[gateway] val coordinate: Target,
  // A missing replacement name means the schema element is hidden.
  private[gateway] val renamed: Option[String]
)

/**
 * Constructors for gateway schema transformations.
 *
 * Gateway `hide*` transformations correspond to the `Exclude*` terminology used by core schema transformers such as
 * `caliban.transformers.Transformer.ExcludeField`, `caliban.transformers.Transformer.ExcludeInputField`, and
 * `caliban.transformers.Transformer.ExcludeArgument`. Gateway transformations additionally rewrite downstream
 * operations and responses so hidden or renamed subgraph coordinates remain routable.
 */
object SchemaTransformation {

  /**
   * Renames a non-operation type.
   */
  def renameType(name: String, renamed: String): SchemaTransformation =
    new SchemaTransformation(TypeTarget(name), Some(renamed))

  /**
   * Hides a type from the composed client schema.
   */
  def hideType(name: String): SchemaTransformation =
    new SchemaTransformation(TypeTarget(name), None)

  /**
   * Renames a field on an object or interface.
   */
  def renameField(typeName: String, name: String, renamed: String): SchemaTransformation =
    new SchemaTransformation(FieldTarget(typeName, name), Some(renamed))

  /**
   * Hides a field from the composed client schema.
   */
  def hideField(typeName: String, name: String): SchemaTransformation =
    new SchemaTransformation(FieldTarget(typeName, name), None)

  /**
   * Renames a field argument.
   */
  def renameArgument(typeName: String, field: String, name: String, renamed: String): SchemaTransformation =
    new SchemaTransformation(ArgumentTarget(typeName, field, name), Some(renamed))

  /**
   * Hides an optional field argument from the composed client schema.
   */
  def hideArgument(typeName: String, field: String, name: String): SchemaTransformation =
    new SchemaTransformation(ArgumentTarget(typeName, field, name), None)

  /**
   * Hides an optional input-object field from the composed client schema.
   */
  def hideInputField(typeName: String, name: String): SchemaTransformation =
    new SchemaTransformation(InputFieldTarget(typeName, name), None)

}
