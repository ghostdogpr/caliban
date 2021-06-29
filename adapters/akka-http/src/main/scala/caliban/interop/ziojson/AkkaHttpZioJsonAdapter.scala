package caliban.interop.ziojson

import caliban.AkkaHttpAdapter
import de.heikoseeberger.akkahttpziojson.ZioJsonSupport

/**
 * An "all-included" mixin for akka-http caliban API with zio-json backend. Mixes in an `adapter` value.
 * <br/>
 * Requires `"de.heikoseeberger" %% "akka-http-zio-json"` to be on the classpath (checked at compile-time).
 *
 * @see [[AkkaHttpAdapter]] for usage example
 */
trait AkkaHttpZioJsonAdapter extends ZioJsonSupport {
  val adapter: AkkaHttpAdapter = AkkaHttpAdapter(new ZioJsonBackend)
}
