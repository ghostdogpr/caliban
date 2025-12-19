package caliban.federation.v2x.connect

case class HTTPHeaderMapping(
  name: String,
  from: Option[String],
  value: Option[String]
)
