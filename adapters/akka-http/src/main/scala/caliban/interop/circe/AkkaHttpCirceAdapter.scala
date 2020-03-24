package caliban.interop.circe

import caliban.AkkaHttpAdapter
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport

/**
 * An "all-included" mixin for akka-http caliban API with circe json backend. Mixes in an `adapter` value.
 * <br/>
 * Requires `"de.heikoseeberger" %% "akka-http-circe"` to be on the classpath (checked at compile-time).
 *
 * @see [[AkkaHttpAdapter]] for usage example
 */
trait AkkaHttpCirceAdapter extends FailFastCirceSupport {
  val adapter: AkkaHttpAdapter = AkkaHttpAdapter(new CirceJsonBackend)
}
