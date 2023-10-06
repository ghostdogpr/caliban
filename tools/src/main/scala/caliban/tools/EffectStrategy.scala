package caliban.tools

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition.FieldDefinition

/**
 * When using codegen to generate server-side code from graphql schemas, it's not possible
 *  to infer which fields in a given object type needs further side-effects.
 *
 * [[EffectStrategy]] allows you to choose at codegen-time.
 *
 * Note that if you call codegen yourself instead of going through the sbt plugin, you're
 * free to provide other implementations of this, for instance based on field names.
 */
trait EffectStrategy {
  def decide(owner: TypeDefinition, field: FieldDefinition): EffectStrategy.Maybe
}

object EffectStrategy {
  sealed trait Maybe
  object Effectful extends Maybe
  object Pure      extends Maybe

  /**
   * Fields are pure unless tagged with directive `@foo`, where the name of `foo` is customizable and provided in `directiveName``
   */
  case class EffectfulIfTaggedWith(directiveName: String) extends EffectStrategy {
    override def decide(owner: TypeDefinition, field: FieldDefinition): Maybe =
      if (field.directives.exists(d => d.name == directiveName)) Effectful else Pure
  }

  /**
   * Fields are effectful unless tagged with directive `@foo`, where the name of `foo` is customizable and provided in `directiveName``
   */
  case class PureIfTaggedWith(directiveName: String) extends EffectStrategy {
    override def decide(owner: TypeDefinition, field: FieldDefinition): Maybe =
      if (field.directives.exists(d => d.name == directiveName)) Pure else Effectful
  }

  object AlwaysPure extends EffectStrategy {
    override def decide(owner: TypeDefinition, field: FieldDefinition): Maybe = EffectStrategy.Pure
  }

  object AlwaysEffectful extends EffectStrategy {
    override def decide(owner: TypeDefinition, field: FieldDefinition): Maybe = EffectStrategy.Effectful
  }
}
