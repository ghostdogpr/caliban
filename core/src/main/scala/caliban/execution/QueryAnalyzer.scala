package caliban.execution

import caliban.{ CalibanError, GraphQL }
import caliban.CalibanError.ValidationError
import zio.{ IO, ZIO }

object QueryAnalyzer {

  /**
   * A query analyzer is a function that takes a root [[Field]] and returns a new root [[Field]] or fails with a [[CalibanError]].
   * In case of failure, the query will be rejected before execution.
   * The environment `R` can be used to "inject" some data that will be used by the resolvers (e.g. query cost).
   */
  type QueryAnalyzer[-R] = Field => ZIO[R, CalibanError, Field]

  /**
   * Attaches to the given GraphQL API definition a function that checks that each query depth is under a given max.
   * @param maxDepth the max allowed depth for a query
   * @param api a GraphQL API definition
   * @return a new GraphQL API definition
   */
  def maxDepth[R, E](maxDepth: Int)(api: GraphQL[R]): GraphQL[R] =
    api.withQueryAnalyzer(checkMaxDepth(maxDepth))

  /**
   * Checks that the given field's depth is under a given max
   * @param maxDepth the max allowed depth for the field
   */
  def checkMaxDepth(maxDepth: Int): QueryAnalyzer[Any] = { field =>
    val depth = calculateDepth(field)
    if (depth > maxDepth) IO.fail(ValidationError(s"Query is too deep: $depth. Max depth: $maxDepth.", ""))
    else IO.succeed(field)
  }

  def calculateDepth(field: Field): Int = {
    val children      = field.fields ++ field.conditionalFields.values.flatten
    val childrenDepth = if (children.isEmpty) 0 else children.map(calculateDepth).max
    childrenDepth + (if (field.name.nonEmpty) 1 else 0)
  }

  /**
   * Attaches to the given GraphQL API definition a function that checks that each query has a limited number of fields.
   * @param maxFields the max allowed number of fields for a query
   * @param api a GraphQL API definition
   * @return a new GraphQL API definition
   */
  def maxFields[R, E](maxFields: Int)(api: GraphQL[R]): GraphQL[R] =
    api.withQueryAnalyzer(checkMaxFields(maxFields))

  /**
   * Checks that the given field has a limited number of fields
   * @param maxFields the max allowed number of fields inside the given field
   */
  def checkMaxFields(maxFields: Int): QueryAnalyzer[Any] = { field =>
    val fields = countFields(field)
    if (fields > maxFields) IO.fail(ValidationError(s"Query has too many fields: $fields. Max fields: $maxFields.", ""))
    else IO.succeed(field)
  }

  def countFields(field: Field): Int =
    innerFields(field.fields) + (if (field.conditionalFields.isEmpty) 0
                                 else field.conditionalFields.values.map(innerFields).max)

  private def innerFields(fields: List[Field]): Int = fields.length + fields.map(countFields).sum

}
