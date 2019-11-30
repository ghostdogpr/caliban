package caliban.execution

import caliban.{ CalibanError, GraphQL }
import caliban.CalibanError.ValidationError
import zio.{ IO, ZIO }

object QueryAnalyzer {

  type QueryAnalyzer[-R] = Field => ZIO[R, CalibanError, Field]

  def maxDepth[R, Q, M, S, E](maxDepth: Int)(interpreter: GraphQL[R, Q, M, S, E]): GraphQL[R, Q, M, S, E] =
    interpreter.withQueryAnalyzer(checkMaxDepth(maxDepth))

  def calculateDepth(field: Field): Int = {
    val children      = field.fields ++ field.conditionalFields.values.flatten
    val childrenDepth = if (children.isEmpty) 0 else children.map(calculateDepth).max
    childrenDepth + (if (field.name.nonEmpty) 1 else 0)
  }

  def checkMaxDepth(maxDepth: Int): QueryAnalyzer[Any] = { field =>
    val depth = calculateDepth(field)
    if (depth > maxDepth) IO.fail(ValidationError(s"Query is too deep: $depth. Max depth: $maxDepth.", ""))
    else IO.succeed(field)
  }

  def maxFields[R, Q, M, S, E](maxFields: Int)(interpreter: GraphQL[R, Q, M, S, E]): GraphQL[R, Q, M, S, E] =
    interpreter.withQueryAnalyzer(checkMaxFields(maxFields))

  private def innerFields(fields: List[Field]): Int = fields.length + fields.map(countFields).sum

  def countFields(field: Field): Int =
    innerFields(field.fields) + (if (field.conditionalFields.isEmpty) 0
                                 else field.conditionalFields.values.map(innerFields).max)

  def checkMaxFields(maxFields: Int): QueryAnalyzer[Any] = { field =>
    val fields = countFields(field)
    if (fields > maxFields) IO.fail(ValidationError(s"Query has too many fields: $fields. Max fields: $maxFields.", ""))
    else IO.succeed(field)
  }

}
