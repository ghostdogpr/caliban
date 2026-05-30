package caliban.validation

import caliban.introspection.adt._
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt._
import caliban.syntax._
import caliban.validation.Utils._

import scala.collection.compat._
import scala.collection.mutable

private[caliban] object FieldMap {
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

  def make(
    context: Context,
    parentType: __Type,
    selectionSet: Iterable[Selection]
  ): collection.Map[String, Set[SelectedField]] = {
    val fields: FM = mutable.HashMap.empty
    loop(context, parentType, selectionSet)(fields)
    fields
  }

  private def loop(
    context: Context,
    parentType: __Type,
    selectionSet: Iterable[Selection]
  )(implicit fields: FM): Unit = {
    val it = selectionSet.iterator
    while (it.hasNext)
      it.next() match {
        case f: Field                                       =>
          addField(f, parentType)
        case FragmentSpread(name, _)                        =>
          context.fragments.getOrElseNull(name) match {
            case null       => ()
            case definition =>
              val typ = getType(Some(definition.typeCondition), parentType, context)
              loop(context, typ, definition.selectionSet)
          }
        case InlineFragment(typeCondition, _, selectionSet) =>
          val typ = getType(typeCondition, parentType, context)
          loop(context, typ, selectionSet)
      }
  }

  private def addField(
    f: Field,
    parentType: __Type
  )(implicit self: FM): Unit =
    parentType.getFieldOrNull(f.name) match {
      case null => ()
      case f1   =>
        val responseName = f.alias.getOrElse(f.name)
        val sf           = SelectedField(parentType, f, f1)
        self.updateWith(responseName) {
          case Some(s) => Some(s + sf)
          case _       => Some(Set.empty + sf)
        }
    }

}
