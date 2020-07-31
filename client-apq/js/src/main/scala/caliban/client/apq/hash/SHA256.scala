package caliban.client.apq.hash

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

@js.native
@JSImport("hash.js", "sha256")
private[hash] class SHA256() extends js.Object {
  def update(msg: Uint8Array): Unit = js.native
  def digest(enc: String): String   = js.native
}
