package caliban.schema

import scala.annotation.StaticAnnotation

trait AnnotationsVersionSpecific {

  /**
   * Annotation that can be used on a case class method to mark it as a GraphQL field.
   * The method must be public, a `def` (does not work on `val`s / `lazy val`s) and must not take any arguments.
   *
   * NOTE: This annotation is not safe for use with ahead-of-time compilation (e.g., generating a GraalVM native-image executable)
   */
  case class GQLField() extends StaticAnnotation

}
