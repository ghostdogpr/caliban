package caliban.gateway.internal

import caliban.parsing.adt.{ Directive, Document }
import caliban.parsing.adt.Definition.{ TypeSystemDefinition, TypeSystemExtension }
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension.SchemaExtension
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension._
import caliban.parsing.adt.Type.NamedType

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/**
 * Fingerprints schema content without source locations or unordered declaration-list ordering.
 * Directive applications and input list values keep their order. The original document is never modified.
 */
private[gateway] object SchemaFingerprint {
  def apply(document: Document): String = {
    // Merging extensions appends directives in source order, independently for each target.
    val extensionDirectives = document.definitions.collect {
      case SchemaExtension(directives, _, _, _)          => None       -> directives
      case ScalarTypeExtension(name, directives)         => Some(name) -> directives
      case ObjectTypeExtension(name, _, directives, _)   => Some(name) -> directives
      case InterfaceTypeExtension(name, directives, _)   => Some(name) -> directives
      case UnionTypeExtension(name, directives, _)       => Some(name) -> directives
      case EnumTypeExtension(name, directives, _)        => Some(name) -> directives
      case InputObjectTypeExtension(name, directives, _) => Some(name) -> directives
    }.filter(_._2.nonEmpty).groupBy(_._1).map { case (target, extensions) =>
      target -> extensions.flatMap(_._2)
    }
    fingerprint((document.definitions, extensionDirectives))
  }

  private def fingerprint(value: Any): String = {
    val digest = MessageDigest.getInstance("SHA-256")

    def token(value: String): Unit = {
      val bytes = value.getBytes(StandardCharsets.UTF_8)
      digest.update(ByteBuffer.allocate(4).putInt(bytes.length).array())
      digest.update(bytes)
    }

    def visitProduct(product: Product): Unit = {
      token(product.productPrefix)
      token(product.productArity.toString)
      product.productIterator.foreach(visit)
    }

    def visit(value: Any): Unit = value match {
      case directive: Directive               =>
        token("Directive")
        visit(directive.name)
        visit(directive.arguments)
        visit(directive.isIntrospectable)
      case values: scala.collection.Map[_, _] =>
        token("Map")
        token(values.size.toString)
        values.toList.sortBy(_._1.toString).foreach { case (key, item) => visit(key); visit(item) }
      case values: scala.collection.Set[_]    =>
        token("Set")
        token(values.size.toString)
        values.toList.sortBy(_.toString).foreach(visit)
      case values: Iterable[_]                =>
        val unordered = values.nonEmpty && values.forall(isSchemaDeclaration)
        token(if (unordered) "Declarations" else "Sequence")
        token(values.size.toString)
        if (unordered) values.iterator.map(fingerprint).toList.sorted.foreach(token)
        else values.foreach(visit)
      case union: UnionTypeDefinition         => visitProduct(union.copy(memberTypes = union.memberTypes.sorted))
      case union: UnionTypeExtension          => visitProduct(union.copy(memberTypes = union.memberTypes.sorted))
      case product: Product                   =>
        visitProduct(product)
      case other                              =>
        token(other.getClass.getName)
        token(other.toString)
    }

    visit(value)
    digest.digest().map(byte => f"${byte & 0xff}%02x").mkString
  }

  private def isSchemaDeclaration(value: Any): Boolean = value match {
    case _: TypeSystemDefinition | _: TypeSystemExtension | _: FieldDefinition | _: InputValueDefinition |
        _: EnumValueDefinition | _: NamedType =>
      true
    case _ => false
  }
}
