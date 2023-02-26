package caliban

import caliban.federation.v2.{ FederationDirectivesV2_3, FederationV2, Versions }

package object federation {

  lazy val v1   = new FederationV1 with FederationDirectives
  lazy val v2_0 = new FederationV2(List(Versions.v2_0))
  lazy val v2_1 = new FederationV2(List(Versions.v2_1))
  lazy val v2_3 = new FederationV2(List(Versions.v2_3)) with FederationDirectivesV2_3

}
