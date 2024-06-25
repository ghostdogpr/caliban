package caliban.schema

import caliban.parsing.adt.Directive

import scala.annotation.StaticAnnotation

trait AnnotationsVersionSpecific {

  /**
   * Annotation used to provide directives to a schema type
   */
  class GQLDirective(val directive: Directive) extends StaticAnnotation

  object GQLDirective {
    def unapply(annotation: GQLDirective): Option[Directive] =
      Some(annotation.directive)
  }

}
