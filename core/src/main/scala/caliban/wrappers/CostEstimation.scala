package caliban.wrappers

import caliban.CalibanError.ValidationError
import caliban.{ InputValue, Value }
import caliban.InputValue.ListValue
import caliban.Value.{ FloatValue, IntValue, StringValue }
import caliban.execution.{ ExecutionRequest, Field }
import caliban.parsing.adt.{ Directive, Document }
import caliban.schema.Types
import caliban.wrappers.Wrapper.ValidationWrapper
import zio.{ IO, URIO, ZIO }

import scala.annotation.tailrec

object CostEstimation {

  final val COST_DIRECTIVE_NAME = "cost"

  object CostDirective {
    def apply(cost: Double): Directive =
      Directive(COST_DIRECTIVE_NAME, arguments = Map("weight" -> FloatValue(cost)))

    def apply(cost: Double, multipliers: List[String]): Directive =
      Directive(
        COST_DIRECTIVE_NAME,
        arguments = Map("weight" -> FloatValue(cost), "multipliers" -> ListValue(multipliers.map(StringValue)))
      )
  }

  /**
   * Computes field cost by examining the @cost directive
   */
  val costDirective = (f: Field) => {
    def computeDirectiveCost(directives: List[Directive]) =
      directives.collectFirst {
        case d if d.name == COST_DIRECTIVE_NAME =>
          // TODO should we support string backed formulas?
          // TODO Support list arguments / multipliers
          val weight = d.arguments
            .get("weight")
            .collect {
              case i: IntValue   => i.toInt.toDouble
              case f: FloatValue => f.toDouble
            }
            .getOrElse(1.0)

          val multipliers = d.arguments
            .get("multipliers")
            .toList
            .collect { case ListValue(values) =>
              values.collect {
                case StringValue(name) =>
                  f.arguments.get(name).collectFirst {
                    case i: IntValue   => i.toInt
                    case d: FloatValue => d.toDouble
                  }
                case _                 => None
              }.flatten
            }
            .flatten
            .sum max 1.0

          weight * multipliers
      }

    val directiveCost = computeDirectiveCost(f.directives).getOrElse(1.0)

    val typeCost = Types
      .innerType(f.fieldType)
      .directives
      .flatMap(computeDirectiveCost)
      .getOrElse(0.0)

    directiveCost + typeCost
  }

  private def computeComplexity(field: Field)(f: Field => Double): Double = {
    @tailrec
    def go(fields: List[Field], total: Double): Double = fields match {
      case Nil          => total
      case head :: tail => go(head.fields ++ tail, f(head) + total)
    }

    go(List(field), 0.0)
  }

  def maxComplexity(maxCost: Int)(f: Field => Double): ValidationWrapper[Any] =
    new ValidationWrapper[Any] {
      override def wrap[R1 <: Any](
        process: Document => ZIO[R1, ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, ValidationError, ExecutionRequest] =
        (doc: Document) =>
          for {
            req <- process(doc)
            cost = computeComplexity(req.field)(f)
            _   <- IO.when(cost > maxCost)(
                     IO.fail(ValidationError(s"Query costs too much: $cost. Max cost: $maxCost.", ""))
                   )
          } yield req
    }

  private def computeComplexityZIO[R](value: Field)(f: Field => URIO[R, Double]): URIO[R, Double] = {
    def loop(field: Field, currentCost: Double): URIO[R, Double] =
      URIO.foldLeft(field.fields)(currentCost)((acc, a) => f(a).map(_ + acc))

    loop(value, 0.0)
  }

  def maxComplexityZIO[R](maxCost: Double)(f: Field => URIO[R, Double]): ValidationWrapper[R] =
    new ValidationWrapper[R] {
      override def wrap[R1 <: R](
        process: Document => ZIO[R1, ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, ValidationError, ExecutionRequest] =
        (doc: Document) =>
          for {
            req  <- process(doc)
            cost <- computeComplexityZIO(req.field)(f)
            _    <- IO.when(cost > maxCost)(
                      IO.fail(ValidationError(s"Query costs too much: $cost. Max cost: $maxCost.", ""))
                    )
          } yield req
    }

}
