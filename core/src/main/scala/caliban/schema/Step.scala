package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.Value.NullValue
import caliban.execution.{ Field, FieldInfo }
import caliban.{ InputValue, PathValue, ResponseValue }
import zio.query.ZQuery
import zio.stream.ZStream

sealed trait Step[-R]

object Step {
  case class ListStep[-R](steps: List[Step[R]])                         extends Step[R]
  case class FunctionStep[-R](step: Map[String, InputValue] => Step[R]) extends Step[R]
  case class MetadataFunctionStep[-R](step: Field => Step[R])           extends Step[R]
  case class QueryStep[-R](query: ZQuery[R, Throwable, Step[R]])        extends Step[R]
  case class StreamStep[-R](inner: ZStream[R, Throwable, Step[R]])      extends Step[R]

  case class ObjectStep[-R](name: String, fields: String => Step[R]) extends Step[R]
  object ObjectStep {
    def apply[R](name: String, fields: Map[String, Step[R]]): ObjectStep[R] =
      new ObjectStep[R](name, fields.getOrElse(_, NullStep))
  }

  object FailureStep {
    def apply(error: Throwable): Step[Any] = QueryStep(ZQuery.fail(error))
  }

  // PureStep is both a Step and a ReducedStep so it is defined outside this object
  // This is to avoid boxing/unboxing pure values during step reduction
  type PureStep = caliban.schema.PureStep
  val PureStep: caliban.schema.PureStep.type = caliban.schema.PureStep

  val NullStep: PureStep = PureStep(NullValue)

  /**
   * Create a Step that fails with the provided error
   */
  def fail(error: Throwable): Step[Any] = FailureStep(error)

  /**
   * Create a Step that fails with the provided error message
   */
  def fail(errorMessage: String): Step[Any] = FailureStep(ExecutionError(errorMessage))

  /**
   * Create a Step that either succeeds with the provided ResponseValue or fails with the provided error
   */
  def fromEither(either: Either[Throwable, ResponseValue]): Step[Any] = either match {
    case Right(value) => PureStep(value)
    case Left(error)  => FailureStep(error)
  }

  /**
   * Merge 2 root steps. Root steps are supposed to be objects so we ignore other cases.
   */
  def mergeRootSteps[R](step1: Step[R], step2: Step[R]): Step[R] = (step1, step2) match {
    case (MetadataFunctionStep(l), MetadataFunctionStep(r))  => MetadataFunctionStep(f => mergeRootSteps(l(f), r(f)))
    case (MetadataFunctionStep(l), r)                        => MetadataFunctionStep(f => mergeRootSteps(l(f), r))
    case (l, MetadataFunctionStep(r))                        => MetadataFunctionStep(f => mergeRootSteps(l, r(f)))
    case (FunctionStep(l), FunctionStep(r))                  => FunctionStep(args => mergeRootSteps(l(args), r(args)))
    case (FunctionStep(l), r)                                => FunctionStep(args => mergeRootSteps(l(args), r))
    case (l, FunctionStep(r))                                => FunctionStep(args => mergeRootSteps(l, r(args)))
    case (ObjectStep(name, fields1), ObjectStep(_, fields2)) => ObjectStep(name, mergeObjectSteps(fields1, fields2))
    // if only step1 is an object, keep it
    case (ObjectStep(_, _), _)                               => step1
    // otherwise keep step2
    case _                                                   => step2
  }

  // fields2 override fields1 in case of conflict
  private def mergeObjectSteps[R](fields1: String => Step[R], fields2: String => Step[R]): String => Step[R] =
    (s: String) =>
      fields2(s) match {
        case NullStep => fields1(s)
        case step     => step
      }
}

sealed abstract class ReducedStep[-R] { self =>
  def isPure: Boolean
}

object ReducedStep {
  case class ListStep[-R](steps: List[ReducedStep[R]], areItemsNullable: Boolean, isPure: Boolean)
      extends ReducedStep[R]

  case class ObjectStep[-R](fields: List[(String, ReducedStep[R], FieldInfo)], hasPureFields: Boolean, isPure: Boolean)
      extends ReducedStep[R]

  case class QueryStep[-R](query: ZQuery[R, ExecutionError, ReducedStep[R]]) extends ReducedStep[R] {
    def isPure: Boolean = false
  }

  case class StreamStep[-R](inner: ZStream[R, ExecutionError, ReducedStep[R]]) extends ReducedStep[R] {
    def isPure: Boolean = false
  }

  case class DeferStep[-R](
    obj: ReducedStep[R],
    deferred: List[(ReducedStep[R], Option[String])],
    path: List[PathValue]
  ) extends ReducedStep[R] {
    def isPure: Boolean = false
  }

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
case class PureStep(value: ResponseValue) extends ReducedStep[Any] with Step[Any] {
  def isPure: Boolean = true
}
