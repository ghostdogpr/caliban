package caliban.federation.connect

sealed trait Method {
  def url: String
  def body: Option[JSONSelection] = None
}

object Method {
  case class GET(url: String)                                             extends Method
  case class DELETE(url: String)                                          extends Method
  case class POST(url: String, override val body: Option[JSONSelection])  extends Method
  case class PUT(url: String, override val body: Option[JSONSelection])   extends Method
  case class PATCH(url: String, override val body: Option[JSONSelection]) extends Method
}
