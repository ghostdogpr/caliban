package models

import caliban.schema.{ ArgBuilder, Schema }

case class CaseStudyArgs(caseNumber: ID)

object CaseStudyArgs {
  implicit val schema: Schema[Any, CaseStudyArgs]    = Schema.gen
  implicit val argBuilder: ArgBuilder[CaseStudyArgs] = ArgBuilder.gen
}
