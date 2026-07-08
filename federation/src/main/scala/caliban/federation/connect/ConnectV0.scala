package caliban.federation.connect

import caliban.federation.v2x.{ Import, Link }

object ConnectV0 {
  val connectUrl = "https://specs.apollo.dev/connect"

  val connect: Link = Link(
    url = s"$connectUrl/v0.1",
    `import` = List(Import("@connect"), Import("@source"))
  )

  val connect0_2: Link = connect.copy(
    url = s"$connectUrl/v0.2"
  )

  val connect0_3: Link = connect0_2.copy(
    url = s"$connectUrl/v0.3"
  )

  val connect0_4: Link = connect0_3.copy(
    url = s"$connectUrl/v0.4"
  )

}
