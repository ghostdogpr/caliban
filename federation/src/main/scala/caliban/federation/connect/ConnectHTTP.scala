package caliban.federation.connect

case class ConnectHTTP(
  method: Method,
  headers: List[HTTPHeaderMapping] = Nil
)
