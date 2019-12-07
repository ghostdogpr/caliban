package caliban.schema

import scala.annotation.StaticAnnotation

object Annotations {

  /**
   * Annotation used to indicate a type or a field is deprecated.
   */
  case class GQLDeprecated(reason: String) extends StaticAnnotation

  /**
   * Annotation used to provide a description to a field or a type.
   */
  case class GQLDescription(value: String) extends StaticAnnotation

  /**
   * Annotation used to customize the name of an input type.
   * This is usually needed to avoid a name clash when a type is used both as an input and an output.
   */
  case class GQLInputName(name: String) extends StaticAnnotation

  /**
   * Annotation used to provide an alternative name to a field or a type.
   */
  case class GQLName(value: String) extends StaticAnnotation
}
