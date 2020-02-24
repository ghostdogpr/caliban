package caliban.derivation.annotation

import scala.annotation.StaticAnnotation

/**
 * Annotation used to indicate that a class member should not be included in the derived Schema
 */
class GQLExclude extends StaticAnnotation
