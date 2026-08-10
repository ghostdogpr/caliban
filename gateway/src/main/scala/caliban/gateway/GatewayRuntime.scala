package caliban.gateway

import caliban.{ CalibanError, GraphQLInterpreter }

/**
 * An executable gateway created by [[Gateway.build]].
 *
 * A runtime may be shared across fibers and used anywhere a [[caliban.GraphQLInterpreter]] is
 * accepted. Its lifetime is bounded by the scope in which it was built.
 */
trait GatewayRuntime[-R] extends GraphQLInterpreter[R, CalibanError]
