package caliban.federation.v2x.connect

case class ConnectHTTP(
  method: Method,
  headers: List[HTTPHeaderMapping] = Nil
)
