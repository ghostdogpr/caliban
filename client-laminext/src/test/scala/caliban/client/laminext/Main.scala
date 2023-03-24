package caliban.client.laminext

import com.raquo.laminar.api.L._
import org.scalajs.dom

object Main {
  def main(args: Array[String]): Unit = {
    val _ = documentEvents(_.onDomContentLoaded).foreach { _ =>
      val appContainer = dom.document.querySelector("#app")
      appContainer.innerHTML = ""
      val _            = render(appContainer, Page.view)
    }(unsafeWindowOwner)
  }
}
