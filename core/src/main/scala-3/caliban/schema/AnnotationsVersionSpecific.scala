package caliban.schema

import scala.annotation.StaticAnnotation

trait AnnotationsVersionSpecific {

  /**
   * Annotation that can be used on a case class method to mark it as a GraphQL field.
   * The method must not take any arguments.
   *
   * NOTE: This annotation is not safe for use with GraalVM native-image
   */
  case class GQLField() extends StaticAnnotation

}
