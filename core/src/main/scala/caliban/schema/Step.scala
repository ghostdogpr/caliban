package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.Value.NullValue
import caliban.execution.{ Field, FieldInfo }
import caliban.{ InputValue, ResponseValue }
import zio.stream.ZStream
import zio.query.ZQuery

sealed trait Step[-R]

object Step {
  case class ListStep[-R](steps: List[Step[R]])                         extends Step[R]
  case class FunctionStep[-R](step: Map[String, InputValue] => Step[R]) extends Step[R]
  case class MetadataFunctionStep[-R](step: Field => Step[R])           extends Step[R]
  case class ObjectStep[-R](name: String, fields: Map[String, Step[R]]) extends Step[R]
  case class QueryStep[-R](query: ZQuery[R, Throwable, Step[R]])        extends Step[R]
  case class StreamStep[-R](inner: ZStream[R, Throwable, Step[R]])      extends Step[R]

  // PureStep is both a Step and a ReducedStep so it is defined outside this object
  // This is to avoid boxing/unboxing pure values during step reduction
  type PureStep = caliban.schema.PureStep
  val PureStep: caliban.schema.PureStep.type = caliban.schema.PureStep

  val NullStep: PureStep = PureStep(NullValue)

  /**
   * Merge 2 root steps. Root steps are supposed to be objects so we ignore other cases.
   */
  def mergeRootSteps[R](step1: Step[R], step2: Step[R]): Step[R] = (step1, step2) match {
    case (ObjectStep(name, fields1), ObjectStep(_, fields2)) =>
      ObjectStep(name, fields1 ++ fields2) // fields2 override fields1 in case of conflict
    case (ObjectStep(_, _), _)                               => step1 // if only step1 is an object, keep it
    case _                                                   => step2 // otherwise keep step2
  }
}

sealed trait ReducedStep[-R] { self =>
  def isPure: Boolean =
    self match {
      case _: PureStep => true
      case _           => false
    }
}

object ReducedStep {
  case class ListStep[-R](steps: List[ReducedStep[R]], areItemsNullable: Boolean) extends ReducedStep[R]
  case class ObjectStep[-R](fields: List[(String, ReducedStep[R], FieldInfo)])    extends ReducedStep[R]
  case class QueryStep[-R](query: ZQuery[R, ExecutionError, ReducedStep[R]])      extends ReducedStep[R]
  case class StreamStep[-R](inner: ZStream[R, ExecutionError, ReducedStep[R]])    extends ReducedStep[R]
  case class DeferStep[-R](
    obj: ReducedStep[R],
    deferred: List[(ReducedStep[R], Option[String])],
    path: List[Either[String, Int]]
  ) extends ReducedStep[R]

  // PureStep is both a Step and a ReducedStep so it is defined outside this object
  // This is to avoid boxing/unboxing pure values during step reduction
  type PureStep = caliban.schema.PureStep
  val PureStep: caliban.schema.PureStep.type = caliban.schema.PureStep
}

/**
 * Represents the step of getting a pure response value without any effect.
 * [[PureStep]] is both a [[Step]] and a [[ReducedStep]] to avoid boxing/unboxing pure values during step reduction.
 *
 * @param value the response value to return for that step
 */
case class PureStep(value: ResponseValue) extends Step[Any] with ReducedStep[Any]
