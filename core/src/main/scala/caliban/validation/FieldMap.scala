package caliban.validation

import caliban.introspection.adt._
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt._
import Utils._

object FieldMap {
  val empty: FieldMap = Map.empty

  implicit class FieldMapOps(val self: FieldMap) extends AnyVal {
    def |+|(that: FieldMap): FieldMap =
      (self.keySet ++ that.keySet).map { k =>
        k -> (self.get(k).getOrElse(Set.empty) ++ that.get(k).getOrElse(Set.empty))
      }.toMap

    def show =
      self.map { case (k, fields) =>
        s"$k -> ${fields.map(_.fieldDef.name).mkString(", ")}"
      }.mkString("\n")

    def addField(
      f: Field,
      parentType: __Type,
      selection: Field
    ): FieldMap = {
      val responseName = f.alias.getOrElse(f.name)

      getFields(parentType)
        .flatMap(fields => fields.find(_.name == f.name))
        .map { f =>
          val sf    = SelectedField(parentType, selection, f)
          val entry = self.get(responseName).map(_ + sf).getOrElse(Set(sf))
          self + (responseName -> entry)
        }
        .getOrElse(self)
    }
  }

  def apply(context: Context, parentType: __Type, selectionSet: Iterable[Selection]): FieldMap =
    selectionSet.foldLeft(FieldMap.empty)({ case (fields, selection) =>
      selection match {
        case FragmentSpread(name, directives)               =>
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
    })
}
