package caliban.federation.v2

import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective

trait FederationDirectivesV2_3 {
  case class GQLInterfaceObject() extends GQLDirective(InterfaceObject)

  val InterfaceObject = Directive("interfaceObject")

}
