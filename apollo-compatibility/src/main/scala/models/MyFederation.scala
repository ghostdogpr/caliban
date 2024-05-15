package models

import caliban.federation.v2x._

abstract class MyFederation
    extends caliban.federation.v2x.FederationV2(
      Versions.v2_3 :: Link(
        "https://myspecs.dev/myCustomDirective/v1.0",
        List(
          Import("@custom")
        )
      ) :: ComposeDirective("@custom") :: Nil
    )
    with FederationDirectivesV2_3
