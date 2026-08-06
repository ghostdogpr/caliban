package caliban.validation

import caliban.introspection.adt._
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt._
import caliban.syntax._
import caliban.validation.Utils._

import scala.collection.compat._
import scala.collection.mutable

private[caliban] object FieldMap {
  final case class Collected(
    fields: collection.Map[String, mutable.ArrayBuffer[SelectedField]],
    fragmentNames: List[String]
  )

  @deprecated("Kept for bin-compatibility only", "3.1.3")
  val empty: FieldMap = Map.empty

  @deprecated("Kept for bin-compatibility only", "3.1.3")
  implicit class FieldMapOps(val self: FieldMap) extends AnyVal {
    def |+|(that: FieldMap): FieldMap = {
      val mb = Map.newBuilder[String, Set[SelectedField]]
      (self.keySet ++ that.keySet).foreach { k =>
        mb += k -> {
          (self.get(k), that.get(k)) match {
            case (Some(s1), Some(s2)) => s1 ++ s2
            case (Some(s1), None)     => s1
            case (None, Some(s2))     => s2
            case _                    => Set.empty[SelectedField]
          }
        }
      }
      mb.result()
    }

    def show: String =
      self.map { case (k, fields) =>
        s"$k -> ${fields.map(_.fieldDef.name).mkString(", ")}"
      }.mkString("\n")

    def addField(
      f: Field,
      parentType: __Type,
      selection: Field
    ): FieldMap = {
      val responseName = f.alias.getOrElse(f.name)

      parentType.getFieldOrNull(f.name) match {
        case null => self
        case f1   =>
          val sf    = SelectedField(parentType, selection, f1)
          val entry = self.get(responseName).map(_ + sf).getOrElse(Set(sf))
          self + (responseName -> entry)
      }
    }
  }

  private type FM = mutable.HashMap[String, Set[SelectedField]]

  private abstract class SelectionCollector {
    final def addField(field: Field, parentType: __Type): Unit =
      parentType.getFieldOrNull(field.name) match {
        case null     => ()
        case fieldDef =>
          appendField(
            field.alias.getOrElse(field.name),
            SelectedField(parentType, field, fieldDef)
          )
      }

    protected def appendField(responseName: String, field: SelectedField): Unit

    def addFragmentSpread(context: Context, parentType: __Type, name: String): Unit
  }

  private final class InliningCollector(fields: FM) extends SelectionCollector {
    override protected def appendField(responseName: String, field: SelectedField): Unit =
      fields.updateWith(responseName) {
        case Some(existing) => Some(existing + field)
        case None           => Some(Set(field))
      }

    override def addFragmentSpread(context: Context, parentType: __Type, name: String): Unit =
      context.fragments.getOrElseNull(name) match {
        case null       => ()
        case definition =>
          val typ = getType(Some(definition.typeCondition), parentType, context)
          loop(context, typ, definition.selectionSet, this)
      }
  }

  private final class FragmentPreservingCollector extends SelectionCollector {
    private val fields        = mutable.HashMap.empty[String, mutable.ArrayBuffer[SelectedField]]
    private val fragmentNames = mutable.ListBuffer.empty[String]
    private val seenFragments = mutable.HashSet.empty[String]

    override protected def appendField(responseName: String, field: SelectedField): Unit =
      fields.getOrElseUpdate(responseName, mutable.ArrayBuffer.empty) += field

    override def addFragmentSpread(_context: Context, _parentType: __Type, name: String): Unit =
      if (seenFragments.add(name)) fragmentNames += name

    def result(): Collected = Collected(fields, fragmentNames.toList)
  }

  @deprecated("Kept for bin-compatibility only", "3.1.3")
  def apply(context: Context, parentType: __Type, selectionSet: Iterable[Selection]): FieldMap =
    make(context, parentType, selectionSet).toMap

  def make(
    context: Context,
    parentType: __Type,
    selectionSet: Iterable[Selection]
  ): collection.Map[String, Set[SelectedField]] = {
    val fields: FM = mutable.HashMap.empty
    loop(context, parentType, selectionSet, new InliningCollector(fields))
    fields
  }

  def collect(
    context: Context,
    parentType: __Type,
    selectionSet: Iterable[Selection]
  ): Collected = {
    val collector = new FragmentPreservingCollector
    loop(context, parentType, selectionSet, collector)
    collector.result()
  }

  private def loop(
    context: Context,
    parentType: __Type,
    selectionSet: Iterable[Selection],
    collector: SelectionCollector
  ): Unit = {
    val it = selectionSet.iterator
    while (it.hasNext)
      it.next() match {
        case f: Field                                       =>
          collector.addField(f, parentType)
        case FragmentSpread(name, _)                        =>
          collector.addFragmentSpread(context, parentType, name)
        case InlineFragment(typeCondition, _, selectionSet) =>
          val typ = getType(typeCondition, parentType, context)
          loop(context, typ, selectionSet, collector)
      }
  }

}
