package caliban.federation.v2

import caliban.InputValue.ListValue
import caliban.Value.StringValue
import caliban.federation.FederationSupport
import caliban.parsing.adt.Directive

class FederationV2
    extends FederationSupport(
      Nil,
      List(
        Directive(
          "link",
          arguments = Map(
            "url"    -> StringValue("https://specs.apollo.dev/federation/v2.0"),
            "import" -> ListValue(
              List(
                StringValue("@key"),
                StringValue("@requires"),
                StringValue("@provides"),
                StringValue("@external"),
                StringValue("@shareable"),
                StringValue("@tag"),
                StringValue("@inaccessible"),
                StringValue("@override")
              )
            )
          )
        )
      )
    )
