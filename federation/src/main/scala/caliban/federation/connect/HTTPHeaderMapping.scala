package caliban.federation.connect

case class HTTPHeaderMapping(
  name: String,
  from: Option[String],
  value: Option[String]
)
