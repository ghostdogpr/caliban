package models

import caliban.parsing.adt.Directive
import caliban.schema.Annotations.GQLDirective

case class Custom() extends GQLDirective(Directive("custom"))
