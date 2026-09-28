package caliban.gateway

/**
 * Describes how a subgraph recalls an object through an ordinary GraphQL query field.
 */
sealed trait Lookup {
  private[gateway] def typeName: String
  private[gateway] def keyFields: List[String]
  private[gateway] def field: String
}

object Lookup {

  /**
   * Describes a lookup field that returns one object for one key using deterministically ordered argument mappings.
   */
  def single(typeName: String, keyFields: List[String], field: String, arguments: (String, Argument[Key])*): Lookup =
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
    arguments: (String, Argument[Batch])*
  ): Lookup =
    ByKey(typeName, keyFields, field, arguments.toList, correlation)

  /**
   * A declarative lookup-argument mapping whose leaves are `Key` reads for a single lookup and `Batch` lists for a
   * list lookup.
   */
  sealed trait Argument[+A] {
    private[gateway] def leaves: List[A] =
      this match {
        case Argument.Leaf(value)           => value :: Nil
        case Argument.ObjectMapping(fields) => fields.flatMap(_._2.leaves)
      }

    private[gateway] def map[B](f: A => B): Argument[B] =
      this match {
        case Argument.Leaf(value)           => Argument.Leaf(f(value))
        case Argument.ObjectMapping(fields) =>
          Argument.ObjectMapping(fields.map { case (name, value) => name -> value.map(f) })
      }
  }

  object Argument {

    /**
     * Reads one declared key field from an object being recalled.
     */
    def key(field: String): Argument[Key] = Leaf(Key(field))

    /**
     * Builds a compound GraphQL input object.
     */
    def obj[A](fields: (String, Argument[A])*): Argument[A] = ObjectMapping(fields.toList)

    /**
     * Builds a list by evaluating the nested mapping once for every batched key.
     */
    def batch(value: Argument[Key]): Argument[Batch] = Leaf(Batch(value))

    private[gateway] final case class Leaf[+A](value: A)                                     extends Argument[A]
    private[gateway] final case class ObjectMapping[+A](fields: List[(String, Argument[A])]) extends Argument[A]
  }

  /**
   * A read of one declared key field, the leaf of a single lookup's argument mappings.
   */
  final case class Key private[gateway] (private[gateway] val field: String)

  /**
   * A list with one key mapping per batched object, the leaf of a list lookup's argument mappings.
   */
  final case class Batch private[gateway] (private[gateway] val value: Argument[Key])

  private[gateway] final case class Single(
    typeName: String,
    keyFields: List[String],
    field: String,
    arguments: List[(String, Argument[Key])]
  ) extends Lookup

  private[gateway] final case class ByKey(
    typeName: String,
    keyFields: List[String],
    field: String,
    arguments: List[(String, Argument[Batch])],
    correlation: Map[String, String]
  ) extends Lookup
}
