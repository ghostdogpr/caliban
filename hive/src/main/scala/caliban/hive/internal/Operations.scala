package caliban.hive.internal

import caliban.InputValue
import caliban.Value.{ EnumValue, FloatValue, IntValue, NullValue, StringValue }
import caliban.execution.Field
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.{ Directive, Document, Selection, VariableDefinition }
import caliban.rendering.DocumentRenderer

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import scala.collection.immutable.ListMap

/**
 * What Hive learns about an operation: its normalized text, its name and the schema coordinates it used.
 */
private[hive] object Operations {

  /**
   * The operation text reported to Hive, normalized like Hive's JS client (`normalizeOperation`): string, int and
   * float literals become `""` and `0`, aliases are dropped, and arguments, selections, variable definitions,
   * definitions and the directives of fragments are sorted. Values written into a query never leave the server, and
   * executions that differ only in them share one entry.
   */
  def normalize(document: Document): String = {
    def literal(value: InputValue): InputValue = value match {
      case _: IntValue | _: FloatValue    => IntValue(0)
      case _: StringValue                 => StringValue("")
      case InputValue.ListValue(values)   => InputValue.ListValue(values.map(literal))
      case InputValue.ObjectValue(fields) => InputValue.ObjectValue(fields.map { case (k, v) => k -> literal(v) })
      case other                          => other
    }

    def arguments(values: Map[String, InputValue]): Map[String, InputValue] =
      ListMap(values.toList.sortBy(_._1).map { case (name, v) => name -> literal(v) }: _*)

    def directives(ds: List[Directive], sorted: Boolean): List[Directive] = {
      val normalized = ds.map(d => d.copy(arguments = arguments(d.arguments)))
      if (sorted) normalized.sortBy(_.name) else normalized
    }

    def variables(vs: List[VariableDefinition]): List[VariableDefinition] =
      vs.map(v =>
        v.copy(defaultValue = v.defaultValue.map(literal), directives = directives(v.directives, sorted = false))
      ).sortBy(_.name)

    def selections(ss: List[Selection]): List[Selection] =
      ss.map {
        case f: Selection.Field          =>
          f.copy(
            alias = None,
            arguments = arguments(f.arguments),
            directives = directives(f.directives, sorted = false),
            selectionSet = selections(f.selectionSet)
          )
        case s: Selection.FragmentSpread => s.copy(directives = directives(s.directives, sorted = true))
        case i: Selection.InlineFragment =>
          i.copy(dirs = directives(i.dirs, sorted = true), selectionSet = selections(i.selectionSet))
      }.sortBy {
        case f: Selection.Field          => (0, f.name)
        case s: Selection.FragmentSpread => (1, s.name)
        case _: Selection.InlineFragment => (2, "")
      }

    val definitions = document.definitions.map {
      case op: OperationDefinition      =>
        op.copy(
          variableDefinitions = variables(op.variableDefinitions),
          directives = directives(op.directives, sorted = false),
          selectionSet = selections(op.selectionSet)
        )
      case fragment: FragmentDefinition =>
        fragment.copy(
          directives = directives(fragment.directives, sorted = true),
          selectionSet = selections(fragment.selectionSet)
        )
      case other                        => other
    }.sortBy {
      case fragment: FragmentDefinition => (0, fragment.name)
      case op: OperationDefinition      => (1, op.name.getOrElse(""))
      case _                            => (2, "")
    }
    DocumentRenderer.renderCompact(document.copy(definitions = definitions))
  }

  /**
   * The name Hive files the operation under: the requested one, or the name of the document's only operation.
   */
  def name(document: Document, requested: Option[String]): Option[String] =
    requested.orElse(document.definitions.collect { case op: OperationDefinition => op } match {
      case op :: Nil => op.name
      case _         => None
    })

  /**
   * The schema coordinates an execution used, from Caliban's typed field tree, following Hive's JS client
   * (`collect-schema-coordinates.ts`): `Type.field`, `Type.field.arg` (plus `Type.field.arg!` for a non-null value),
   * the scalar type of each given value, `Enum.VALUE` and `Input.field` / `Input.field!`. Variables are already
   * resolved in that tree, so their values count like literals. Introspection fields are left out, and so are fields
   * that `@skip` or `@include` removed from this execution.
   */
  def coordinates(root: Field): Set[String] = {
    val out = Set.newBuilder[String]

    def input(tpe: __Type, value: InputValue): Unit = {
      val t = tpe.innerType
      value match {
        case NullValue                                                           => ()
        case InputValue.ListValue(values)                                        => values.foreach(input(t, _))
        case InputValue.ObjectValue(fields) if t.kind == __TypeKind.INPUT_OBJECT =>
          t.name.foreach { typeName =>
            fields.foreach { case (name, v) =>
              out += s"$typeName.$name"
              if (v != NullValue) out += s"$typeName.$name!"
              Option(t.getInputFieldOrNull(name)).foreach(d => input(d._type, v))
            }
          }
        case EnumValue(v) if t.kind == __TypeKind.ENUM                           => t.name.foreach(n => out += s"$n.$v")
        case StringValue(v) if t.kind == __TypeKind.ENUM                         => t.name.foreach(n => out += s"$n.$v")
        case _ if t.kind == __TypeKind.SCALAR                                    => t.name.foreach(out += _)
        case _                                                                   => ()
      }
    }

    def visit(field: Field): Unit =
      field.fields.foreach { child =>
        child.parentType.map(_.innerType).foreach { parent =>
          parent.name.filterNot(n => n.startsWith("__") || child.name.startsWith("__")).foreach { parentName =>
            val coordinate = s"$parentName.${child.name}"
            out += coordinate
            val definition = Option(parent.getFieldOrNull(child.name))
            child.arguments.foreach { case (name, value) =>
              out += s"$coordinate.$name"
              if (value != NullValue) out += s"$coordinate.$name!"
              definition.flatMap(_.allArgs.find(_.name == name)).foreach(arg => input(arg._type, value))
            }
          }
        }
        visit(child)
      }

    visit(root)
    out.result()
  }

  /**
   * The key of an operation in a report. The coordinates are part of it: variables are resolved in the field tree, so
   * one document can use different coordinates per execution, and a key per document would file them all under the
   * first execution's coordinates. Hive's JS client keys on the coordinates for the same reason.
   */
  def key(body: String, name: Option[String], coordinates: Set[String]): String = {
    val material = List(body, name.getOrElse(""), coordinates.toList.sorted.mkString(";")).mkString("\u0000")
    val digest   = MessageDigest.getInstance("SHA-256").digest(material.getBytes(UTF_8))
    digest.iterator.take(16).map(b => f"${b & 0xff}%02x").mkString
  }
}
