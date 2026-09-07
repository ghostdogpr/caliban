package caliban

import caliban.federation.v2x.{
  FederationDirectivesV2_10,
  FederationDirectivesV2_11,
  FederationDirectivesV2_12,
  FederationDirectivesV2_3,
  FederationDirectivesV2_5,
  FederationDirectivesV2_6,
  FederationDirectivesV2_7,
  FederationDirectivesV2_8,
  FederationDirectivesV2_9,
  FederationV2,
  Versions
}

package object federation {

  lazy val v1    = new FederationV1 with FederationDirectives with FederationDirectivesV1
  lazy val v2_0  = new FederationV2(List(Versions.v2_0))
  lazy val v2_1  = new FederationV2(List(Versions.v2_1))
  lazy val v2_2  = new FederationV2(List(Versions.v2_2))
  lazy val v2_3  = new FederationV2(List(Versions.v2_3)) with FederationDirectivesV2_3
  lazy val v2_4  = new FederationV2(List(Versions.v2_4)) with FederationDirectivesV2_3
  lazy val v2_5  = new FederationV2(List(Versions.v2_5)) with FederationDirectivesV2_5
  lazy val v2_6  = new FederationV2(List(Versions.v2_6)) with FederationDirectivesV2_6
  lazy val v2_7  = new FederationV2(List(Versions.v2_7)) with FederationDirectivesV2_6 with FederationDirectivesV2_7
  lazy val v2_8  = new FederationV2(List(Versions.v2_8)) with FederationDirectivesV2_8
  lazy val v2_9  = new FederationV2(List(Versions.v2_9)) with FederationDirectivesV2_9
  lazy val v2_10 = new FederationV2(List(Versions.v2_10)) with FederationDirectivesV2_10
  lazy val v2_11 = new FederationV2(List(Versions.v2_11)) with FederationDirectivesV2_11
  lazy val v2_12 = new FederationV2(List(Versions.v2_12)) with FederationDirectivesV2_12
  lazy val v2_13 = new FederationV2(List(Versions.v2_13)) with FederationDirectivesV2_12
  lazy val v2_14 = new FederationV2(List(Versions.v2_14)) with FederationDirectivesV2_12
  lazy val v2_15 = new FederationV2(List(Versions.v2_15)) with FederationDirectivesV2_12
}
