package models

import caliban.schema.{ ArgBuilder, Schema }

case class ProductResearchArgs(study: CaseStudyArgs)

object ProductResearchArgs {
  implicit val schema: Schema[Any, ProductResearchArgs]    = Schema.gen
  implicit val argBuilder: ArgBuilder[ProductResearchArgs] = ArgBuilder.gen
}
