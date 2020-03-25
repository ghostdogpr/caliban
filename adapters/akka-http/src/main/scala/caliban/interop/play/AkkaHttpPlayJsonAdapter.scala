package caliban.interop.play

import caliban.AkkaHttpAdapter
import de.heikoseeberger.akkahttpplayjson.PlayJsonSupport

/**
 * An "all-included" mixin for akka-http caliban API with play-json backend. Mixes in an `adapter` value.
 * <br/>
 * Requires `"de.heikoseeberger" %% "akka-http-play-json"` to be on the classpath (checked at compile-time).
 *
 * @see [[AkkaHttpAdapter]] for usage example
 */
trait AkkaHttpPlayJsonAdapter extends PlayJsonSupport {
  val adapter: AkkaHttpAdapter = AkkaHttpAdapter(new PlayJsonBackend)
}
