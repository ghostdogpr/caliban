package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.introspection.adt._
import caliban.parsing.adt.Selection
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.syntax._
import caliban.validation.Utils._

import java.util.IdentityHashMap
import scala.collection.mutable

object FragmentValidator {
  private sealed trait ComparisonMode {
    def covers(requested: ComparisonMode): Boolean
    def findNameOrArgumentsConflict(first: SelectedField, second: SelectedField): Option[String]
  }

  private object ComparisonMode {
    // Comparing overlapping parents is stronger because it also checks field names and arguments.
    case object Overlapping extends ComparisonMode {
      override def covers(requested: ComparisonMode): Boolean = true

      override def findNameOrArgumentsConflict(
        first: SelectedField,
        second: SelectedField
      ): Option[String] =
        if (first.fieldDef.name != second.fieldDef.name)
          Some(
            s"${first.parentType.name.getOrElse("")}.${first.fieldDef.name} and ${second.parentType.name
                .getOrElse("")}.${second.fieldDef.name} are different fields."
          )
        else if (first.selection.arguments != second.selection.arguments)
          Some(s"${first.fieldDef.name} and ${second.fieldDef.name} have different arguments")
        else None
    }

    case object MutuallyExclusive extends ComparisonMode {
      override def covers(requested: ComparisonMode): Boolean = requested eq MutuallyExclusive

      override def findNameOrArgumentsConflict(
        _first: SelectedField,
        _second: SelectedField
      ): Option[String] = None
    }

    def exclusiveWhen(condition: Boolean): ComparisonMode =
      if (condition) MutuallyExclusive else Overlapping
  }

  private class ComparisonMemo[A, B] {
    private val data = mutable.HashMap.empty[A, mutable.HashMap[B, ComparisonMode]]

    def recordIfNotCovered(first: A, second: B, mode: ComparisonMode): Boolean = {
      val seconds = data.getOrElseUpdate(first, mutable.HashMap.empty)
      seconds.get(second) match {
        case Some(previousMode) if previousMode.covers(mode) => false
        case _                                               =>
          seconds.update(second, mode)
          true
      }
    }
  }

  private final class FragmentPairMemo {
    private val pairs = new ComparisonMemo[String, String]

    def recordIfNotCovered(first: String, second: String, mode: ComparisonMode): Boolean =
      if (first.compareTo(second) <= 0) pairs.recordIfNotCovered(first, second, mode)
      else pairs.recordIfNotCovered(second, first, mode)
  }

  private final case class FieldMapId(value: Int) extends AnyVal
  private final case class CachedFields(id: FieldMapId, collected: FieldMap.Collected)

  def findConflictsWithinSelectionSet(
    context: Context,
    parentType: __Type,
    selectionSet: List[Selection]
  ): Either[ValidationError, Unit] =
    new ConflictFinder(context, parentType).find(selectionSet)

  private final class ConflictFinder(context: Context, rootParentType: __Type) {
    private val comparedFragmentPairs          = new FragmentPairMemo
    private val comparedFieldsAndFragmentPairs = new ComparisonMemo[FieldMapId, String]
    private val fieldsCache                    =
      new IdentityHashMap[List[Selection], IdentityHashMap[__Type, CachedFields]]()
    private var nextFieldMapId                 = 0

    def find(selectionSet: List[Selection]): Either[ValidationError, Unit] =
      validateSelectionSets(rootParentType, selectionSet, mutable.HashSet.empty) match {
        case Some(conflict) => Left(ValidationError(conflict, ""))
        case None           => ValidationOps.unit
      }

    private def getFieldsAndFragmentNames(parentType: __Type, selectionSet: List[Selection]): CachedFields = {
      var byParentType = fieldsCache.get(selectionSet)
      if (byParentType eq null) {
        byParentType = new IdentityHashMap[__Type, CachedFields]()
        fieldsCache.put(selectionSet, byParentType)
      }

      var cached = byParentType.get(parentType)
      if (cached eq null) {
        cached = CachedFields(FieldMapId(nextFieldMapId), FieldMap.collect(context, parentType, selectionSet))
        nextFieldMapId += 1
        byParentType.put(parentType, cached)
      }
      cached
    }

    private def getReferencedFieldsAndFragmentNames(fragmentName: String): CachedFields = {
      val definition = context.fragments.getOrElseNull(fragmentName)
      if (definition eq null) null
      else {
        val fragmentType = getType(definition.typeCondition, context).getOrElse(rootParentType)
        getFieldsAndFragmentNames(fragmentType, definition.selectionSet)
      }
    }

    private def doTypesConflict(t1: __Type, t2: __Type): Boolean =
      if (isNonNull(t1))
        if (isNonNull(t2)) t1.ofType.flatMap(p1 => t2.ofType.map(p2 => doTypesConflict(p1, p2))).getOrElse(true)
        else true
      else if (isNonNull(t2))
        true
      else if (isListType(t1))
        if (isListType(t2)) t1.ofType.flatMap(p1 => t2.ofType.map(p2 => doTypesConflict(p1, p2))).getOrElse(true)
        else true
      else if (isListType(t2))
        true
      else if (isLeafType(t1) && isLeafType(t2))
        t1.name != t2.name
      else if (!isComposite(t1) || !isComposite(t2))
        true
      else
        false

    private def findConflict(
      responseName: String,
      f1: SelectedField,
      f2: SelectedField,
      parentMode: ComparisonMode
    ): Option[String] = {
      val mode = ComparisonMode.exclusiveWhen(
        (parentMode eq ComparisonMode.MutuallyExclusive) ||
          (isObjectType(f1.parentType) && isObjectType(f2.parentType) && f1.parentType.name != f2.parentType.name)
      )

      if (doTypesConflict(f1.fieldDef._type, f2.fieldDef._type))
        Some(
          s"$responseName has conflicting types: ${f1.parentType.name.getOrElse("")}.${f1.fieldDef.name} and ${f2.parentType.name
              .getOrElse("")}.${f2.fieldDef.name}. Try using an alias."
        )
      else
        mode.findNameOrArgumentsConflict(f1, f2) match {
          case conflict @ Some(_) => conflict
          case None               =>
            if (f1.selection.selectionSet.nonEmpty && f2.selection.selectionSet.nonEmpty)
              findConflictsBetweenSubSelectionSets(
                mode,
                f1.fieldDef._type.innerType,
                f1.selection.selectionSet,
                f2.fieldDef._type.innerType,
                f2.selection.selectionSet
              )
            else None
        }
    }

    private def collectConflictsWithin(fields: CachedFields): Option[String] = {
      val fieldGroups = fields.collected.fields.iterator
      while (fieldGroups.hasNext) {
        val (responseName, values) = fieldGroups.next()
        var i                      = 0
        while (i < values.length - 1) {
          var j = i + 1
          while (j < values.length) {
            val conflict = findConflict(responseName, values(i), values(j), ComparisonMode.Overlapping)
            if (conflict.nonEmpty) return conflict
            j += 1
          }
          i += 1
        }
      }
      None
    }

    private def collectConflictsBetween(
      fields1: CachedFields,
      fields2: CachedFields,
      mode: ComparisonMode
    ): Option[String] =
      if (fields1.id == fields2.id) None
      else {
        val fieldGroups = fields1.collected.fields.iterator
        while (fieldGroups.hasNext) {
          val (responseName, values1) = fieldGroups.next()
          fields2.collected.fields.get(responseName) match {
            case Some(values2) =>
              var i = 0
              while (i < values1.length) {
                var j = 0
                while (j < values2.length) {
                  val conflict = findConflict(responseName, values1(i), values2(j), mode)
                  if (conflict.nonEmpty) return conflict
                  j += 1
                }
                i += 1
              }
            case None          => ()
          }
        }
        None
      }

    private def collectConflictsBetweenFieldsAndFragment(
      fields: CachedFields,
      fragmentName: String,
      mode: ComparisonMode
    ): Option[String] =
      if (!comparedFieldsAndFragmentPairs.recordIfNotCovered(fields.id, fragmentName, mode)) None
      else {
        val fragmentFields = getReferencedFieldsAndFragmentNames(fragmentName)
        if ((fragmentFields eq null) || fields.id == fragmentFields.id) None
        else {
          val directConflict = collectConflictsBetween(fields, fragmentFields, mode)
          if (directConflict.nonEmpty) directConflict
          else {
            val nestedFragments = fragmentFields.collected.fragmentNames
            var remaining       = nestedFragments
            while (remaining.nonEmpty) {
              val conflict = collectConflictsBetweenFieldsAndFragment(fields, remaining.head, mode)
              if (conflict.nonEmpty) return conflict
              remaining = remaining.tail
            }
            None
          }
        }
      }

    private def collectConflictsBetweenFragments(
      fragmentName1: String,
      fragmentName2: String,
      mode: ComparisonMode
    ): Option[String] =
      if (fragmentName1 == fragmentName2) None
      else if (!comparedFragmentPairs.recordIfNotCovered(fragmentName1, fragmentName2, mode)) None
      else {
        val fields1 = getReferencedFieldsAndFragmentNames(fragmentName1)
        val fields2 = getReferencedFieldsAndFragmentNames(fragmentName2)
        if ((fields1 eq null) || (fields2 eq null)) None
        else {
          val directConflict = collectConflictsBetween(fields1, fields2, mode)
          if (directConflict.nonEmpty) directConflict
          else {
            var nestedFragments = fields2.collected.fragmentNames
            while (nestedFragments.nonEmpty) {
              val conflict =
                collectConflictsBetweenFragments(fragmentName1, nestedFragments.head, mode)
              if (conflict.nonEmpty) return conflict
              nestedFragments = nestedFragments.tail
            }

            nestedFragments = fields1.collected.fragmentNames
            while (nestedFragments.nonEmpty) {
              val conflict =
                collectConflictsBetweenFragments(nestedFragments.head, fragmentName2, mode)
              if (conflict.nonEmpty) return conflict
              nestedFragments = nestedFragments.tail
            }
            None
          }
        }
      }

    private def findConflictsBetweenSubSelectionSets(
      mode: ComparisonMode,
      parentType1: __Type,
      selectionSet1: List[Selection],
      parentType2: __Type,
      selectionSet2: List[Selection]
    ): Option[String] = {
      val fields1        = getFieldsAndFragmentNames(parentType1, selectionSet1)
      val fields2        = getFieldsAndFragmentNames(parentType2, selectionSet2)
      val directConflict = collectConflictsBetween(fields1, fields2, mode)
      if (directConflict.nonEmpty) directConflict
      else {
        var fragmentNames = fields2.collected.fragmentNames
        while (fragmentNames.nonEmpty) {
          val conflict = collectConflictsBetweenFieldsAndFragment(fields1, fragmentNames.head, mode)
          if (conflict.nonEmpty) return conflict
          fragmentNames = fragmentNames.tail
        }

        fragmentNames = fields1.collected.fragmentNames
        while (fragmentNames.nonEmpty) {
          val conflict = collectConflictsBetweenFieldsAndFragment(fields2, fragmentNames.head, mode)
          if (conflict.nonEmpty) return conflict
          fragmentNames = fragmentNames.tail
        }

        var fragmentNames1 = fields1.collected.fragmentNames
        while (fragmentNames1.nonEmpty) {
          var fragmentNames2 = fields2.collected.fragmentNames
          while (fragmentNames2.nonEmpty) {
            val conflict =
              collectConflictsBetweenFragments(fragmentNames1.head, fragmentNames2.head, mode)
            if (conflict.nonEmpty) return conflict
            fragmentNames2 = fragmentNames2.tail
          }
          fragmentNames1 = fragmentNames1.tail
        }
        None
      }
    }

    private def findConflictsWithin(parentType: __Type, selectionSet: List[Selection]): Option[String] = {
      val fields         = getFieldsAndFragmentNames(parentType, selectionSet)
      val directConflict = collectConflictsWithin(fields)
      if (directConflict.nonEmpty) directConflict
      else {
        var fragmentNames1 = fields.collected.fragmentNames
        while (fragmentNames1.nonEmpty) {
          val fragmentName1 = fragmentNames1.head
          val conflict      =
            collectConflictsBetweenFieldsAndFragment(fields, fragmentName1, ComparisonMode.Overlapping)
          if (conflict.nonEmpty) return conflict

          var fragmentNames2 = fragmentNames1.tail
          while (fragmentNames2.nonEmpty) {
            val fragmentConflict =
              collectConflictsBetweenFragments(fragmentName1, fragmentNames2.head, ComparisonMode.Overlapping)
            if (fragmentConflict.nonEmpty) return fragmentConflict
            fragmentNames2 = fragmentNames2.tail
          }
          fragmentNames1 = fragmentNames1.tail
        }
        None
      }
    }

    private def validateSelectionSets(
      parentType: __Type,
      selectionSet: List[Selection],
      validatedFragments: mutable.HashSet[String]
    ): Option[String] = {
      val conflict = findConflictsWithin(parentType, selectionSet)
      if (conflict.nonEmpty) conflict
      else {
        var remaining = selectionSet
        while (remaining.nonEmpty) {
          remaining.head match {
            case field: Field                                       =>
              if (field.selectionSet.nonEmpty) {
                val fieldDef = parentType.getFieldOrNull(field.name)
                if (fieldDef ne null) {
                  val nestedConflict =
                    validateSelectionSets(fieldDef._type.innerType, field.selectionSet, validatedFragments)
                  if (nestedConflict.nonEmpty) return nestedConflict
                }
              }
            case FragmentSpread(name, _)                            =>
              if (validatedFragments.add(name)) {
                val definition = context.fragments.getOrElseNull(name)
                if (definition ne null) {
                  val fragmentType   = getType(definition.typeCondition, context).getOrElse(parentType)
                  val nestedConflict =
                    validateSelectionSets(fragmentType, definition.selectionSet, validatedFragments)
                  if (nestedConflict.nonEmpty) return nestedConflict
                }
              }
            case InlineFragment(typeCondition, _, nestedSelections) =>
              val fragmentType   = getType(typeCondition, parentType, context)
              val nestedConflict = validateSelectionSets(fragmentType, nestedSelections, validatedFragments)
              if (nestedConflict.nonEmpty) return nestedConflict
          }
          remaining = remaining.tail
        }
        None
      }
    }

  }
}
