package caliban.tools

import caliban.InputValue
import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveLocation
import caliban.parsing.adt.Type

sealed trait SchemaComparisonChange {
  def breaking: Boolean = false
}

object SchemaComparisonChange {
  final case class TypeAdded(name: String)                                     extends SchemaComparisonChange {
    override def toString: String = s"Type '$name' was added."
  }
  final case class TypeDeleted(name: String)                                   extends SchemaComparisonChange {
    override def toString: String  = s"Type '$name' was deleted."
    override def breaking: Boolean = true
  }
  final case class TypeKindChanged(typeName: String, from: String, to: String) extends SchemaComparisonChange {
    override def toString: String  = s"Type '$typeName' was changed from kind '$from' to kind '$to'."
    override def breaking: Boolean = true
  }

  final case class EnumValueAdded(typeName: String, value: String)   extends SchemaComparisonChange {
    override def toString: String = s"Enum value '$value' was added on enum type '$typeName'."
  }
  final case class EnumValueDeleted(typeName: String, value: String) extends SchemaComparisonChange {
    override def toString: String  = s"Enum value '$value' was deleted from enum type '$typeName'."
    override def breaking: Boolean = true
  }

  final case class UnionMemberAdded(typeName: String, member: String)   extends SchemaComparisonChange {
    override def toString: String = s"Member '$member' was added on union type '$typeName'."
  }
  final case class UnionMemberDeleted(typeName: String, member: String) extends SchemaComparisonChange {
    override def toString: String  = s"Member value '$member' was deleted from union type '$typeName'."
    override def breaking: Boolean = true
  }

  final case class ObjectImplementsAdded(typeName: String, implements: String)   extends SchemaComparisonChange {
    override def toString: String = s"Object type '$typeName' now implements interface: '$implements'."
  }
  final case class ObjectImplementsDeleted(typeName: String, implements: String) extends SchemaComparisonChange {
    override def toString: String  = s"Object type '$typeName' no longer implements interface: '$implements'."
    override def breaking: Boolean = true
  }

  final case class SchemaQueryTypeChanged(from: Option[String], to: Option[String])    extends SchemaComparisonChange {
    override def toString: String  =
      s"Root query type was changed from ${renderRootType(from)} to ${renderRootType(to)}."
    override def breaking: Boolean = true
  }
  final case class SchemaMutationTypeChanged(from: Option[String], to: Option[String]) extends SchemaComparisonChange {
    override def toString: String  =
      s"Root mutation type was changed from ${renderRootType(from)} to ${renderRootType(to)}."
    override def breaking: Boolean = true
  }
  final case class SchemaSubscriptionTypeChanged(from: Option[String], to: Option[String])
      extends SchemaComparisonChange {
    override def toString: String  =
      s"Root subscription type was changed from ${renderRootType(from)} to ${renderRootType(to)}."
    override def breaking: Boolean = true
  }

  final case class FieldAdded(typeName: String, field: String)   extends SchemaComparisonChange {
    override def toString: String = s"Field '$field' was added on type '$typeName'."
  }
  final case class FieldDeleted(typeName: String, field: String) extends SchemaComparisonChange {
    override def toString: String  = s"Field '$field' was deleted from type '$typeName'."
    override def breaking: Boolean = true
  }

  final case class DirectiveDefinitionAdded(directiveName: String)   extends SchemaComparisonChange {
    override def toString: String = s"Directive '$directiveName' was added."
  }
  final case class DirectiveDefinitionDeleted(directiveName: String) extends SchemaComparisonChange {
    override def toString: String  = s"Directive '$directiveName' was deleted."
    override def breaking: Boolean = true
  }
  final case class DirectiveLocationAdded(location: DirectiveLocation, directiveName: String)
      extends SchemaComparisonChange {
    override def toString: String = s"Location '$location' was added on directive '$directiveName'."
  }
  final case class DirectiveLocationDeleted(location: DirectiveLocation, directiveName: String)
      extends SchemaComparisonChange {
    override def toString: String  = s"Location '$location' was deleted from directive '$directiveName'."
    override def breaking: Boolean = true
  }

  final case class DescriptionAdded(target: Target)   extends SchemaComparisonChange {
    override def toString: String = s"Description was added on $target."
  }
  final case class DescriptionDeleted(target: Target) extends SchemaComparisonChange {
    override def toString: String = s"Description was deleted on $target."
  }
  final case class DescriptionChanged(target: Target) extends SchemaComparisonChange {
    override def toString: String = s"Description was changed on $target."
  }

  final case class DirectiveAdded(directiveName: String, target: Target)   extends SchemaComparisonChange {
    override def toString: String = s"Directive '$directiveName' was added on $target."
  }
  final case class DirectiveDeleted(directiveName: String, target: Target) extends SchemaComparisonChange {
    override def toString: String  = s"Directive '$directiveName' was deleted from $target."
    override def breaking: Boolean = true
  }
  final case class DirectiveArgumentAdded(directiveName: String, argName: String, target: Target)
      extends SchemaComparisonChange {
    override def toString: String = s"Argument '$argName' was added on directive '$directiveName' of $target."
  }
  final case class DirectiveArgumentDeleted(directiveName: String, argName: String, target: Target)
      extends SchemaComparisonChange {
    override def toString: String  = s"Argument '$argName' was deleted from directive '$directiveName' on $target."
    override def breaking: Boolean = true
  }
  final case class DirectiveArgumentChanged(
    directiveName: String,
    argName: String,
    from: InputValue,
    to: InputValue,
    target: Target
  )                                                                        extends SchemaComparisonChange {
    override def toString: String  =
      s"Argument '$argName' was changed from '$from' to '$to' on directive '$directiveName' on $target."
    override def breaking: Boolean = true
  }

  final case class ArgumentAdded(argName: String, target: Target)   extends SchemaComparisonChange {
    override def toString: String = s"Argument '$argName' was added on $target."
  }
  final case class ArgumentDeleted(argName: String, target: Target) extends SchemaComparisonChange {
    override def toString: String  = s"Argument '$argName' was deleted from $target."
    override def breaking: Boolean = true
  }
  final case class ArgumentChanged(argName: String, from: InputValue, to: InputValue, target: Target)
      extends SchemaComparisonChange {
    override def toString: String  = s"Argument '$argName' was changed from '$from' to '$to' on $target."
    override def breaking: Boolean = true
  }

  final case class TypeChanged(from: Type, to: Type, target: Target) extends SchemaComparisonChange {
    override def toString: String  = s"Type was changed from '$from' to '$to' on $target."
    override def breaking: Boolean = true
  }

  sealed trait Target
  object Target {
    final case class Directive(name: String)                    extends Target {
      override def toString: String = s"directive '$name'"
    }
    final case class Type(name: String)                         extends Target {
      override def toString: String = s"type '$name'"
    }
    final case class Field(name: String, typeName: String)      extends Target {
      override def toString: String = s"field '$name' of type '$typeName'"
    }
    final case class EnumValue(value: String, typeName: String) extends Target {
      override def toString: String = s"enum value '$value' of type '$typeName'"
    }
    final case class Argument(name: String, target: Target)     extends Target {
      override def toString: String = s"argument '$name' of $target"
    }
  }

  private def renderRootType(t: Option[String]): String = t.fold("defaults")(t => s"type '$t'")
}
