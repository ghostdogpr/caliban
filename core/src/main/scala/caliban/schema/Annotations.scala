package caliban.schema

import scala.annotation.StaticAnnotation

object Annotations {
  case class GQLName(value: String)        extends StaticAnnotation
  case class GQLDescription(value: String) extends StaticAnnotation
  case class GQLDeprecated(reason: String) extends StaticAnnotation
}
