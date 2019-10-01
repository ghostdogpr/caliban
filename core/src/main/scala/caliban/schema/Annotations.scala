package caliban.schema

import scala.annotation.StaticAnnotation

object Annotations {

  /**
   * Annotation used to provide an alternative name to a field or a type.
   */
  case class GQLName(value: String) extends StaticAnnotation

  /**
   * Annotation used to provide a description to a field or a type.
   */
  case class GQLDescription(value: String) extends StaticAnnotation

  /**
   * Annotation used to indicate a type or a field is deprecated.
   */
  case class GQLDeprecated(reason: String) extends StaticAnnotation
}
