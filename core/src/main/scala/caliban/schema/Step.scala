package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue
import caliban.ResponseValue.NullValue
import caliban.parsing.adt.Value
import zio.ZIO
import zio.stream.ZStream

sealed trait Step[-R]

object Step {
  case class ListStep[-R](steps: List[Step[R]])                         extends Step[R]
  case class FunctionStep[-R](step: Map[String, Value] => Step[R])      extends Step[R]
  case class ObjectStep[-R](name: String, fields: Map[String, Step[R]]) extends Step[R]
  case class DeferredStep[-R](inner: ZIO[R, ExecutionError, Step[R]])   extends Step[R]
  case class StreamStep[-R](inner: ZStream[R, ExecutionError, Step[R]]) extends Step[R]

  // PureStep is both a Step and a ReducedStep so it is defined outside this object
  // This is to avoid boxing/unboxing pure values during step reduction
  type PureStep = caliban.schema.PureStep
  val PureStep: caliban.schema.PureStep.type = caliban.schema.PureStep

  val NullStep: PureStep = PureStep(NullValue)
}

sealed trait ReducedStep[-R]

object ReducedStep {
  case class ListStep[-R](steps: List[ReducedStep[R]])                         extends ReducedStep[R]
  case class ObjectStep[-R](fields: List[(String, ReducedStep[R])])            extends ReducedStep[R]
  case class DeferredStep[-R](inner: ZIO[R, ExecutionError, ReducedStep[R]])   extends ReducedStep[R]
  case class StreamStep[-R](inner: ZStream[R, ExecutionError, ReducedStep[R]]) extends ReducedStep[R]

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
