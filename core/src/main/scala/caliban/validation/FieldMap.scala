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

  @deprecated("Kept for bin-compatibility only", "3.1.3")
  def apply(context: Context, parentType: __Type, selectionSet: Iterable[Selection]): FieldMap =
    make(context, parentType, selectionSet).toMap

  @deprecated("Kept for bin-compatibility only", "3.1.6")
  def make(
    context: Context,
    parentType: __Type,
    selectionSet: Iterable[Selection]
  ): collection.Map[String, Set[SelectedField]] = {
    val fields: FM    = mutable.HashMap.empty
    val seenFragments = mutable.HashSet.empty[String]

    def addSelections(parentType: __Type, selectionSet: Iterable[Selection]): Unit = {
      val collected = collect(context, parentType, selectionSet)
      collected.fields.foreach { case (responseName, selectedFields) =>
        fields.updateWith(responseName) {
          case Some(existing) => Some(existing ++ selectedFields)
          case None           => Some(selectedFields.toSet)
        }
      }
      collected.fragmentNames.foreach { name =>
        if (seenFragments.add(name))
          context.fragments.getOrElseNull(name) match {
            case null       => ()
            case definition =>
              val fragmentType = getType(Some(definition.typeCondition), parentType, context)
              addSelections(fragmentType, definition.selectionSet)
          }
      }
    }

    addSelections(parentType, selectionSet)
    fields
  }

  def collect(
    context: Context,
    parentType: __Type,
    selectionSet: Iterable[Selection]
  ): Collected = {
    val fields        = mutable.HashMap.empty[String, mutable.ArrayBuffer[SelectedField]]
    val fragmentNames = mutable.ListBuffer.empty[String]
    val seenFragments = mutable.HashSet.empty[String]
    collectInto(context, parentType, selectionSet, fields, fragmentNames, seenFragments)
    Collected(fields, fragmentNames.toList)
  }

  private def collectInto(
    context: Context,
    parentType: __Type,
    selectionSet: Iterable[Selection],
    fields: mutable.HashMap[String, mutable.ArrayBuffer[SelectedField]],
    fragmentNames: mutable.ListBuffer[String],
    seenFragments: mutable.HashSet[String]
  ): Unit = {
    val it = selectionSet.iterator
    while (it.hasNext)
      it.next() match {
        case f: Field                                       =>
          parentType.getFieldOrNull(f.name) match {
            case null     => ()
            case fieldDef =>
              val responseName = f.alias.getOrElse(f.name)
              fields.getOrElseUpdate(responseName, mutable.ArrayBuffer.empty) +=
                SelectedField(parentType, f, fieldDef)
          }
        case FragmentSpread(name, _)                        =>
          if (seenFragments.add(name)) fragmentNames += name
        case InlineFragment(typeCondition, _, selectionSet) =>
          val typ = getType(typeCondition, parentType, context)
          collectInto(context, typ, selectionSet, fields, fragmentNames, seenFragments)
      }
  }

}
