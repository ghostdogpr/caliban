package caliban.wrappers

import caliban.CalibanError.ValidationError
import caliban.InputValue.ListValue
import caliban.ResponseValue.ObjectValue
import caliban.Value.{ FloatValue, IntValue, StringValue }
import caliban.execution.{ ExecutionRequest, Field }
import caliban.parsing.adt.{ Directive, Document }
import caliban.schema.Annotations.GQLDirective
import caliban.schema.Types
import caliban.validation.Validator
import caliban.wrappers.Wrapper.{ EffectfulWrapper, OverallWrapper, ValidationWrapper }
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, ResponseValue }
import zio.{ Ref, UIO, URIO, ZIO }

import scala.annotation.tailrec

object CostEstimation {

  final val COST_DIRECTIVE_NAME = "cost"
  final val COST_EXTENSION_NAME = "queryCost"

  case class GQLCost(cost: Double, multipliers: List[String] = Nil)
      extends GQLDirective(CostDirective(cost, multipliers))

  /**
   * A directive that can be applied to both fields and types to flag them as targets for cost analysis.
   * This allows a simple estimation to be applied which can be used to prevent overly expensive queries from being executed
   */
  object CostDirective {
    def apply(cost: Double): Directive =
      Directive(COST_DIRECTIVE_NAME, arguments = Map("weight" -> FloatValue(cost)))

    def apply(cost: Double, multipliers: List[String]): Directive = {
      val multi = multipliers match {
        case Nil => Map.empty
        case _   => Map("multipliers" -> ListValue(multipliers.map(StringValue.apply)))
      }
      Directive(
        COST_DIRECTIVE_NAME,
        arguments = Map("weight" -> FloatValue(cost)) ++ multi
      )
    }
  }

  /**
   * Computes field cost by examining the @cost directive. This can be used in conjunction with `queryCost` or [[maxCost]]
   * In order to compute the estimated cost of executing a query.
   *
   * @note This will be executed *before* the actual resolvers are called, which allows you to stop potentially expensive queries
   *       from being run, but may also require more work on the developer part to determine the correct heuristic for estimating field cost.
   */
  val costDirective: Field => Double = (f: Field) => {
    def computeDirectiveCost(directives: List[Directive]) =
      directives.collectFirst {
        case d if d.name == COST_DIRECTIVE_NAME =>
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
                    case i: IntValue   => i.toInt.toDouble
                    case d: FloatValue => d.toDouble
                  }
                case _                 => None
              }.flatten
            }
            .flatten
            .sum[Double]
            .max(1.0)

          weight * multipliers
      }

    val directiveCost = computeDirectiveCost(f.directives)

    val typeCost = Types
      .innerType(f.fieldType)
      .directives
      .flatMap(computeDirectiveCost)

    (directiveCost orElse typeCost).getOrElse(1.0)
  }

  /**
   * Computes the estimated cost of the query based on the default cost estimation (backed by the @GQLCost directive) and adds it as an extension to the GraphQLResponse.
   * This is useful for tracking the overall cost of a query either when you are trying to dial in a correct heuristic or when you
   * want to inform users of your graph how expensive their queries are.
   *
   * @see queryCostWith, queryCostZIO
   */
  lazy val queryCost: Wrapper[Any] = queryCost(costDirective)

  /**
   * Computes the estimated cost of the query based on the provided field cost function and adds it as an extension to the GraphQLResponse.
   * This is useful for tracking the overall cost of a query either when you are trying to dial in a correct heuristic or when you
   * want to inform users of your graph how expensive their queries are.
   *
   * @see queryCostWith, queryCostZIO
   */
  def queryCost(f: Field => Double): Wrapper[Any] =
    queryCostZIOWrapperState(costWrapper(_)(f))(addCostToExtensions)

  /**
   * Computes the estimated cost of the query based on the provided field cost function and passes it to the second function
   * which can run an arbitrary side effect with the result.
   *
   * @see queryCost
   */
  def queryCostWith[R](f: Field => Double)(p: Double => URIO[R, Any]): Wrapper[R] =
    queryCostZIOWrapperState[R](costWrapper(_)(f))((cost, r) => p(cost).as(r))

  /**
   * A more powerful version of `queryCost` which allows the field cost computation to return an effect instead of a plain value
   * when computing the cost estimate.
   * @param f The field cost estimate function
   *
   * @see queryCostZIOWith for a more powerful version
   */
  def queryCostZIO[R](f: Field => URIO[R, Double]): Wrapper[R] =
    queryCostZIOWrapperState(costWrapperZIO(_)(f))(addCostToExtensions)

  /**
   * A more powerful version of [[queryCostZIO]] that allows the total result of the query to be pushed into a separate effect.
   * This is useful when you want to compute the cost of the query but you already have your own system for recording the cost.
   * @param f The field cost estimate function
   * @param p A function which receives the total estimated cost of executing the query and can
   */
  def queryCostZIOWith[R](f: Field => URIO[R, Double])(p: Double => URIO[R, Any]): Wrapper[R] =
    queryCostZIOWrapperState(costWrapperZIO(_)(f))((cost, r) => p(cost) as r)

  /**
   * The most powerful version of `queryCost` that allows maximum freedom in how the wrapper is defined. The function accepts
   * a function that will take a Ref that can be used for book keeping to track the current cost of the query, and returns a wrapper.
   * Normally this wrapper is a validation wrapper because it should be run before execution for the purposes of cost estimation.
   * However, the API provides the flexibility to redefine that. The second parameter of the function will receive the final cost estimate of the query
   * as well as the current graphql response, it returns an effect that produces a potentially modified response.
   *
   * @param costWrapper User provided function that will result in a wrapper that may be used when computing the cost of a query
   * @param p The response transforming function which receives the cost estimate as well as the response value.
   *
   * @see queryCostZIOWith, queryCostZIO, queryCost
   */
  def queryCostZIOWrapperState[R](
    costWrapper: Ref[Double] => Wrapper[R]
  )(
    p: (Double, GraphQLResponse[CalibanError]) => URIO[R, GraphQLResponse[CalibanError]]
  ): Wrapper[R] = EffectfulWrapper(
    Ref.make(0.0).map { cost =>
      costWrapper(cost) |+| costOverall(resp => cost.get.flatMap(p(_, resp)))
    }
  )

  /**
   * Computes the estimated cost of executing the query using the provided function and compares it to
   * the `maxCost` parameter which determines the maximum allowable cost for a query.
   *
   * @param maxCost The maximum allowable cost for executing a query
   * @param f The field cost estimate function
   */
  def maxCost(maxCost: Double)(f: Field => Double): ValidationWrapper[Any] =
    maxCostOrError(maxCost)(f)(cost => ValidationError(s"Query costs too much: $cost. Max cost: $maxCost.", ""))

  /**
   * More powerful version of [[maxCost]] which allow you to also specify the error that is returned when the cost exceeds the maximum cost.
   * @param maxCost The total cost allowed for any one query
   * @param f The function used to evaluate the cost of a single field
   * @param error A function that will be provided the total estimated cost and must return the error that will be returned
   */
  def maxCostOrError(maxCost: Double)(f: Field => Double)(error: Double => ValidationError): ValidationWrapper[Any] =
    new ValidationWrapper[Any] {
      override def wrap[R1 <: Any](
        process: Document => ZIO[R1, ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, ValidationError, ExecutionRequest] =
        (doc: Document) =>
          process(doc).tap { req =>
            ZIO.unlessZIO(Validator.skipQueryValidationRef.get) {
              val cost = computeCost(req.field)(f)
              ZIO.when(cost > maxCost)(ZIO.fail(error(cost)))
            }
          }
    }

  /**
   * More powerful version of [[maxCost]] which allows the field computation function to specify a function which returns an effectful computation
   * for the cost of a field.
   * @param maxCost The total cost allowed for any one query
   * @param f The function used to evaluate the cost of a single field returning an effect which will result in the field cost as a double
   */
  def maxCostZIO[R](maxCost: Double)(f: Field => URIO[R, Double]): ValidationWrapper[R] =
    new ValidationWrapper[R] {
      override def wrap[R1 <: R](
        process: Document => ZIO[R1, ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, ValidationError, ExecutionRequest] =
        (doc: Document) =>
          process(doc).tap { req =>
            ZIO.unlessZIO(Validator.skipQueryValidationRef.get) {
              computeCostZIO(req.field)(f).flatMap { cost =>
                ZIO.when(cost > maxCost)(
                  ZIO.fail(ValidationError(s"Query costs too much: $cost. Max cost: $maxCost.", ""))
                )
              }
            }
          }
    }

  private def costWrapper(total: Ref[Double])(f: Field => Double): ValidationWrapper[Any] =
    new ValidationWrapper[Any] {
      override def wrap[R1 <: Any](
        process: Document => ZIO[R1, ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, ValidationError, ExecutionRequest] =
        (doc: Document) =>
          for {
            req <- process(doc)
            _   <- total.set(computeCost(req.field)(f))
          } yield req
    }

  private def costWrapperZIO[R](total: Ref[Double])(f: Field => URIO[R, Double]): ValidationWrapper[R] =
    new ValidationWrapper[R] {
      override def wrap[R1 <: R](
        process: Document => ZIO[R1, ValidationError, ExecutionRequest]
      ): Document => ZIO[R1, ValidationError, ExecutionRequest] =
        (doc: Document) =>
          for {
            req  <- process(doc)
            cost <- computeCostZIO(req.field)(f)
            _    <- total.set(cost)
          } yield req
    }

  private def addCostToExtensions(
    cost: Double,
    resp: GraphQLResponse[CalibanError]
  ): UIO[GraphQLResponse[CalibanError]] =
    ZIO.succeed(
      resp.copy(
        extensions = Some(
          ObjectValue(
            resp.extensions.foldLeft[List[(String, ResponseValue)]](
              List(COST_EXTENSION_NAME -> FloatValue(cost))
            )(_ ++ _.fields)
          )
        )
      )
    )

  private def costOverall[R](
    f: GraphQLResponse[CalibanError] => URIO[R, GraphQLResponse[CalibanError]]
  ): OverallWrapper[R] =
    new OverallWrapper[R] {
      override def wrap[R1 <: R](
        process: GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
      ): GraphQLRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
        (req: GraphQLRequest) => process(req).flatMap(f)
    }

  private def computeCost(field: Field)(f: Field => Double): Double = {
    @tailrec
    def go(fields: List[Field], total: Double): Double = fields match {
      case Nil          => total
      case head :: tail => go(head.fields ++ tail, f(head) + total)
    }

    go(List(field), 0.0)
  }

  private def computeCostZIO[R](field: Field)(f: Field => URIO[R, Double]): URIO[R, Double] = {
    @tailrec
    def go(fields: List[Field], result: List[URIO[R, Double]]): List[URIO[R, Double]] = fields match {
      case Nil          => result
      case head :: tail =>
        go(head.fields ++ tail, f(head) :: result)
    }

    ZIO.mergeAllPar(go(List(field), Nil))(0.0)(_ + _)
  }

}
