package caliban.federation

import zio.{ FiberRef, Has, UIO, URIO }

package object tracing {
  type IncludeApolloTracing = Has[FiberRef[IncludeApolloTracing.State]]

  object IncludeApolloTracing {
    sealed trait State { def enabled: Boolean }

    case object Enabled  extends State { val enabled = true  }
    case object Disabled extends State { val enabled = false }

    def make(enabled: State): UIO[FiberRef[State]] =
      FiberRef.make(enabled)
  }

  val disable = URIO.accessM[IncludeApolloTracing](_.get.set(IncludeApolloTracing.Disabled))
  val enable  = URIO.accessM[IncludeApolloTracing](_.get.set(IncludeApolloTracing.Enabled))

  val isTracingEnabled = URIO.accessM[IncludeApolloTracing](_.get.get.map(_.enabled))

}
