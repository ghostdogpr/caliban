package models

import caliban.schema.Schema

case class CaseStudy(
  caseNumber: ID,
  description: Option[String]
)

object CaseStudy {
  implicit val schema: Schema[Any, CaseStudy] = Schema.gen
}
