package models

import caliban.schema.Schema

@GQLKey("study { caseNumber }")
case class ProductResearch(
  study: CaseStudy,
  outcome: Option[String]
)

object ProductResearch {
  implicit val schema: Schema[Any, ProductResearch] = Schema.gen
}
