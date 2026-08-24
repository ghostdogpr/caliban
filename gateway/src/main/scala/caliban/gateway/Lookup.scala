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
   * argument mappings.
   */
  def list(
    typeName: String,
    keyFields: List[String],
    field: String,
    correlation: Correlation,
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

  /**
   * Describes how results from a list lookup correlate to requested keys.
   */
  sealed trait Correlation

  object Correlation {

    /**
     * Correlates results to keys by their list position. The lookup must return one position for every requested key.
     */
    val ordered: Correlation = Ordered

    /**
     * Correlates results by mapping returned fields to declared key fields. The lookup must return non-null list items;
     * missing entities are represented by omission.
     */
    def byKey(responseFieldsToKeyFields: Map[String, String]): Correlation =
      ByKey(responseFieldsToKeyFields)

    private[gateway] case object Ordered                                 extends Correlation
    private[gateway] final case class ByKey(fields: Map[String, String]) extends Correlation
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
    correlation: Correlation
  ) extends Lookup
}
