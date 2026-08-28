package caliban.gateway

/**
 * Describes how a subgraph recalls an object through an ordinary GraphQL query field.
 */
sealed trait Lookup {
  private[gateway] def typeName: String
  private[gateway] def keyFields: List[String]
  private[gateway] def field: String
  private[gateway] def arguments: List[(String, Lookup.Argument)]
}

object Lookup {

  /**
   * Describes a lookup field that returns one object for one key using deterministically ordered argument mappings.
   */
  def single(
    typeName: String,
    keyFields: List[String],
    field: String,
    arguments: (String, Argument)*
  ): Lookup =
    Single(typeName, keyFields, field, arguments.toList)

  /**
   * Describes a lookup field that returns a list of objects for a batch of keys using deterministically ordered
   * argument mappings. Correlation maps returned fields to declared key fields. Results must be non-null;
   * missing entities are omitted.
   */
  def list(
    typeName: String,
    keyFields: List[String],
    field: String,
    correlation: Map[String, String],
    arguments: (String, Argument)*
  ): Lookup =
    ListLookup(typeName, keyFields, field, arguments.toList, correlation)

  /**
   * A declarative lookup-argument mapping.
   */
  sealed trait Argument

  object Argument {

    /**
     * Reads one declared key field from an object being recalled.
     */
    def key(field: String): Argument = Key(field)

    /**
     * Builds a compound GraphQL input object.
     */
    def obj(fields: (String, Argument)*): Argument = ObjectMapping(fields.toList)

    /**
     * Builds a list by evaluating the nested mapping once for every batched key.
     */
    def batch(value: Argument): Argument = Batch(value)

    private[gateway] final case class Key(field: String)                              extends Argument
    private[gateway] final case class ObjectMapping(fields: List[(String, Argument)]) extends Argument
    private[gateway] final case class Batch(value: Argument)                          extends Argument
  }

  private[gateway] final case class Single(
    typeName: String,
    keyFields: List[String],
    field: String,
    arguments: List[(String, Argument)]
  ) extends Lookup

  private[gateway] final case class ListLookup(
    typeName: String,
    keyFields: List[String],
    field: String,
    arguments: List[(String, Argument)],
    correlation: Map[String, String]
  ) extends Lookup
}
