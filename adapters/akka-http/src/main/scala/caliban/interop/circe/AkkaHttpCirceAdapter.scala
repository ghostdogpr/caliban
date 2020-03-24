package caliban.interop.circe

import caliban.AkkaHttpAdapter
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport

trait AkkaHttpCirceAdapter extends FailFastCirceSupport {
  val adapter: AkkaHttpAdapter = AkkaHttpAdapter(new CirceJsonBackend)
}
