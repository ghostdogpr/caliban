package caliban.tools.stitching

import zio._
import zio.query._

import caliban.{ CalibanError, ResponseValue }
import caliban.schema._
import caliban.introspection.adt._

trait PartialRemoteSchema[R0, R, A] { self =>
  def toType(isInput: Boolean, isSubscription: Boolean): __Type

  def resolve(value: A, args: caliban.execution.Field): ZIO[R0, CalibanError, ResponseValue]

  def provideEnvironment[R1 <: R0](env: ZEnvironment[R1]): Schema[R, A] = new Schema[R, A] {
    def resolve(value: A): Step[R] =
      Step.MetadataFunctionStep { (args: caliban.execution.Field) =>
        Step.QueryStep(ZQuery.fromZIO(self.resolve(value, args).map(Step.PureStep.apply).provideEnvironment(env)))
      }

    protected def toType(isInput: Boolean, isSubscription: Boolean): __Type =
      self.toType(isInput, isSubscription)
  }
}
