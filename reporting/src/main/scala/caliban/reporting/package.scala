package caliban

import sttp.client3.SttpBackend
import zio.Task

package object reporting {

  type SttpClient = SttpBackend[Task, Any]
}
