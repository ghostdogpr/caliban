package caliban.validation

import caliban.introspection.adt._
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt._
import caliban.validation.Utils._

import scala.collection.compat._
import scala.collection.mutable

private[caliban] object FieldMap {
  final case class Collected(
    fields: collection.Map[String, mutable.ArrayBuffer[SelectedField]],
    fragmentNames: List[String]
  )

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
