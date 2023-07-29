package caliban.validation

import caliban.introspection.adt._
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt._
import Utils._

object FieldMap {
  val empty: FieldMap = Map.empty

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

      parentType.allFields.collectFirst {
        case f1 if f1.name == f.name =>
          val sf    = SelectedField(parentType, selection, f1)
          val entry = self.get(responseName).map(_ + sf).getOrElse(Set(sf))
          self + (responseName -> entry)
      }
        .getOrElse(self)
    }
  }

  def apply(context: Context, parentType: __Type, selectionSet: Iterable[Selection]): FieldMap =
    selectionSet.foldLeft(FieldMap.empty) { case (fields, selection) =>
      selection match {
        case FragmentSpread(name, _)                        =>
          context.fragments
            .get(name)
            .map { definition =>
              val typ = getType(Some(definition.typeCondition), parentType, context)
              apply(context, typ, definition.selectionSet) |+| fields
            }
            .getOrElse(fields)
        case f: Field                                       =>
          fields.addField(f, parentType, f)
        case InlineFragment(typeCondition, _, selectionSet) =>
          val typ = getType(typeCondition, parentType, context)
          apply(context, typ, selectionSet) |+| fields
      }
    }
}
