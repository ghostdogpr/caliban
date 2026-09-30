package caliban.gateway.internal.planning

import caliban.InputValue
import caliban.execution.Field
import caliban.gateway._
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.composition.DirectiveComposition._
import caliban.gateway.internal.planning.OperationPlan.EntityFetch
import caliban.introspection.adt._
import caliban.parsing.adt.OperationType
import caliban.Value.{ IntValue, NullValue }

import scala.collection.compat._
import scala.collection.mutable

/**
 * Estimates the subgraph operations in one query plan using the GraphQL cost specification.
 */
private[gateway] final class OperationCost(
  types: Map[String, __Type],
  possibleTypesByName: Map[String, Set[String]],
  costMetadata: CostMetadata
) {
  import OperationCost._

  def estimate(plan: OperationPlan): Either[String, Long] = {
    val walk       = new Walk(sized && plan.entities.nonEmpty)
    val rootCost   = plan.roots.foldLeft(BigInt(0)) { (total, fetch) =>
      total + operationBase(plan.operationType) +
        walk.fieldsCost(fetch.downstream.mapConserve(collected), fetch.source, Vector.empty, BigInt(1), Nil)
    }
    // A fetch sizes only paths below its merge path, so shorter merge paths go first.
    val entityCost = plan.entities
      .groupBy(fetch => fetch.root -> fetch.mergePath)
      .toList
      .sortBy(_._1._2.length)
      .foldLeft(BigInt(0)) { case (total, (_, fetches)) => total + walk.entityFetchesCost(fetches) }
    walk.error.toLeft(bounded(rootCost + entityCost))
  }

  private val sized = costMetadata.listSizes.nonEmpty

  private def collected(field: Field): Field =
    if (field.allFieldsUniqueNameAndCondition) {
      val fields = field.fields.mapConserve(collected)
      if (fields eq field.fields) field else field.copy(fields = fields)
    } else
      field.copy(fields = {
        val name          = field.fieldType.innerType.name.getOrElse("")
        val possibleTypes = possibleTypesByName.getOrElse(name, Set(name))
        val fields        = mutable.LinkedHashMap.empty[Field, Set[String]]
        possibleTypes.toList.sorted.foreach { runtime =>
          field.collectFields(runtime).foreach { child =>
            val shared = child.copy(_condition = None)
            fields.update(shared, fields.getOrElse(shared, Set.empty) + runtime)
          }
        }
        fields.iterator.map { case (child, members) =>
          collected(child.copy(_condition = if (members == possibleTypes) None else Some(members)))
        }.toList
      })

  private def operationBase(operation: OperationType): BigInt =
    if (operation == OperationType.Mutation) BigInt(10) else BigInt(0)

  private def conditionalCost[A](values: List[A], conditions: A => Option[Set[String]])(
    cost: (A, Option[String]) => BigInt
  ): BigInt = {
    val (conditional, unconditional) = values.partition(value => conditions(value).nonEmpty)
    val base                         = unconditional.foldLeft(BigInt(0))((total, value) => total + cost(value, None))
    // Only one concrete runtime type can apply, so charge the most expensive branch.
    val branch                       = conditional
      .flatMap(value => conditions(value).toList.flatten)
      .distinct
      .map(runtimeType =>
        conditional.foldLeft(BigInt(0)) { (total, value) =>
          if (conditions(value).exists(_.contains(runtimeType))) total + cost(value, Some(runtimeType)) else total
        }
      )
      .reduceOption(_ max _)
      .getOrElse(BigInt(0))
    base + branch
  }

  private def maximumFieldCost(field: Field)(cost: FieldCostParts => BigInt): BigInt = {
    val parent       = innerParentTypeName(field)
    def declaredCost = {
      val arguments =
        field.parentType.flatMap(tpe => fieldDefinition(tpe.innerType, field.name)).toList.flatMap(_.allArgs)
      cost(fieldOwnCost(parent, field, field.fieldType, arguments))
    }
    implementingParents(field).iterator.flatMap { name =>
      types.get(name).flatMap(fieldDefinition(_, field.name)).map { definition =>
        cost(fieldOwnCost(name, field, definition._type, definition.allArgs))
      }
    }
      .reduceOption(_ max _)
      .getOrElse(declaredCost)
  }

  private def implementingParents(field: Field): Set[String] =
    if (field.parentType.exists(tpe => isAbstractType(tpe.innerType)))
      possibleTypesByName.getOrElse(innerParentTypeName(field), Set.empty)
    else Set.empty

  // Merge paths use response aliases; pending sized paths use schema field names.
  private final class Walk(tracksPaths: Boolean) {
    private val representations = mutable.HashMap.empty[Vector[String], BigInt]
    private val pendingByPath   = mutable.HashMap.empty[Vector[String], List[SizedPath]]
    var error: Option[String]   = None

    def fieldsCost(
      fields: List[Field],
      source: Source,
      base: Vector[String],
      inherited: BigInt,
      paths: List[SizedPath]
    ): BigInt =
      conditionalCost(
        fields.map(field => field._condition -> fieldCost(field, source, base, inherited, paths)),
        (entry: (Option[Set[String]], BigInt)) => entry._1
      )((entry, _) => entry._2)

    def entityFetchesCost(fetches: List[EntityFetch]): BigInt =
      conditionalCost(
        fetches,
        (fetch: EntityFetch) => Some(fetch.fields.flatMap(_._condition.toList.flatten).toSet).filter(_.nonEmpty)
      )(entityFetchCost)

    private def entityFetchCost(fetch: EntityFetch, runtimeType: Option[String]): BigInt = {
      val entity     = types.get(fetch.target.entityType).fold(BigInt(1))(outputTypeCost).max(BigInt(0))
      val fields     =
        runtimeType.fold(fetch.fields)(runtime => fetch.fields.filter(_._condition.forall(_.contains(runtime))))
      val inherited  = representations.getOrElse(fetch.mergePath, BigInt(1))
      val pending    = pendingByPath.getOrElse(fetch.mergePath, Nil)
      val selections =
        fieldsCost(fields.mapConserve(collected), fetch.target.source, fetch.mergePath, inherited, pending)
      inherited * (entity + selections)
    }

    private def fieldCost(
      field: Field,
      source: Source,
      base: Vector[String],
      inherited: BigInt,
      paths: List[SizedPath]
    ): BigInt = {
      val (size, nestedPaths) = sizing(field, source, paths)
      val multiplier          = inherited * size
      val path                = if (tracksPaths) record(base :+ field.aliasedName, multiplier, nestedPaths) else base
      val nested              = fieldsCost(field.fields, source, path, multiplier, nestedPaths)
      maximumFieldCost(field)(_.total(size, nested))
    }

    private def record(path: Vector[String], multiplier: BigInt, nestedPaths: List[SizedPath]): Vector[String] = {
      representations.update(path, representations.get(path).fold(multiplier)(_ max multiplier))
      pendingByPath.update(path, maximumSizedPaths(pendingByPath.getOrElse(path, Nil) ::: nestedPaths))
      path
    }

    private def sizing(field: Field, source: Source, paths: List[SizedPath]): (BigInt, List[SizedPath]) =
      if (!sized) Unsized
      else {
        val definitions            = fieldListSizes(field, source)
        val (activated, remaining) = matchingSizedPaths(field.name, paths).partition(_.path.isEmpty)
        val direct                 = definitions.filter(_.sizedFields.isEmpty).map(resolvedListSize(field, _))
        val declared               = definitions.flatMap { definition =>
          val size = resolvedListSize(field, definition)
          definition.sizedFields.map(SizedPath(_, size))
        }
        if (error.isEmpty && definitions.exists(invalidSlicing(field, _)))
          error = Some(
            s"Exactly one slicing argument must be supplied for field '${innerParentTypeName(field)}.${field.name}'."
          )
        activated.map(_.size).reduceOption(_ max _).orElse(direct.reduceOption(_ max _)).getOrElse(BigInt(1)) ->
          preferSizedPaths(declared, remaining)
      }
  }

  private def invalidSlicing(field: Field, listSize: ListSize): Boolean =
    listSize.requireOneSlicingArgument && listSize.slicingArguments.nonEmpty &&
      listSize.slicingArguments.count(argument => slicingValue(field, argument).nonEmpty) != 1

  // A nearer @listSize replaces an inherited size for the same path.
  private def preferSizedPaths(primary: List[SizedPath], fallback: List[SizedPath]): List[SizedPath] = {
    val preferred      = maximumSizedPaths(primary)
    val preferredPaths = preferred.map(_.path).toSet
    preferred ::: maximumSizedPaths(fallback.filterNot(value => preferredPaths.contains(value.path)))
  }

  private def maximumSizedPaths(values: List[SizedPath]): List[SizedPath] =
    values.groupMapReduce(_.path)(_.size)(_ max _).map { case (path, size) => SizedPath(path, size) }.toList

  private def fieldListSizes(field: Field, source: Source): List[ListSize] =
    if (costMetadata.listSizes.isEmpty) Nil
    else {
      val parent   = innerParentTypeName(field)
      val direct   = costMetadata.listSizes.get(SourceField(source.name, parent, field.name)).toList
      val concrete =
        implementingParents(field).toList.flatMap(name =>
          costMetadata.listSizes.get(SourceField(source.name, name, field.name))
        )
      (direct ::: concrete).distinct
    }

  private def matchingSizedPaths(name: String, paths: List[SizedPath]): List[SizedPath] =
    paths.filter(_.path.headOption.contains(name)).map(value => SizedPath(value.path.drop(1), value.size))

  private def resolvedListSize(field: Field, listSize: ListSize): BigInt =
    listSize.slicingArguments
      .flatMap(argument => slicingValue(field, argument))
      .reduceOption(_ max _)
      .orElse(listSize.assumedSize)
      .getOrElse(BigInt(1))

  private def slicingValue(field: Field, argument: SlicingArgument): Option[BigInt] = {
    val path = argument.path

    def nested(value: InputValue, remaining: List[String]): Option[InputValue] =
      (value, remaining) match {
        case (_, Nil)                                       => Some(value)
        case (InputValue.ObjectValue(values), name :: rest) => values.get(name).flatMap(nested(_, rest))
        case _                                              => None
      }

    field.arguments
      .get(path.head)
      .orElse(argument.defaultValue)
      .flatMap(nested(_, path.tail))
      .flatMap {
        case InputValue.ListValue(values) => Some(BigInt(values.size))
        case NullValue                    => None
        case _ if argument.listValued     => Some(BigInt(1))
        case value: IntValue              => Some(value.toBigInt.max(BigInt(0)))
        case _                            => None
      }
  }

  private def fieldOwnCost(
    parent: String,
    field: Field,
    fieldType: __Type,
    definitions: List[__InputValue]
  ): FieldCostParts = {
    val oneTime = costMetadata.weights.get(FieldCoordinate(parent, field.name)).getOrElse(BigInt(0)) +
      argumentCost(parent, field, definitions)
    FieldCostParts(oneTime.max(BigInt(0)), outputTypeCost(fieldType).max(BigInt(0)))
  }

  private def argumentCost(parent: String, field: Field, arguments: List[__InputValue]): BigInt =
    arguments.foldLeft(BigInt(0)) { (total, argument) =>
      field.arguments.get(argument.name).orElse(argument.parsedDefaultValue) match {
        case Some(value) =>
          val base = costMetadata.weights
            .get(ArgumentCoordinate(parent, field.name, argument.name))
            .getOrElse(namedTypeCost(argument._type.innerType))
          total + base + inputFieldCost(value, argument._type)
        case None        => total
      }
    }

  private def inputFieldCost(value: InputValue, tpe: __Type): BigInt =
    tpe.kind match {
      case __TypeKind.NON_NULL     => tpe.ofType.fold(BigInt(0))(inputFieldCost(value, _))
      case __TypeKind.LIST         =>
        (tpe.ofType, value) match {
          case (Some(element), InputValue.ListValue(values)) =>
            values.foldLeft(BigInt(0))((total, value) => total + inputFieldCost(value, element))
          case (Some(element), value)                        => inputFieldCost(value, element)
          case (None, _)                                     => BigInt(0)
        }
      case __TypeKind.INPUT_OBJECT =>
        value match {
          case InputValue.ObjectValue(values) =>
            tpe.allInputFields.foldLeft(BigInt(0)) { (total, field) =>
              values
                .get(field.name)
                .orElse(field.parsedDefaultValue)
                .fold(total) { nested =>
                  val base = costMetadata.weights
                    .get(InputFieldCoordinate(tpe.name.getOrElse(""), field.name))
                    .getOrElse(namedTypeCost(field._type.innerType))
                  total + base + inputFieldCost(nested, field._type)
                }
            }
          case _                              => BigInt(0)
        }
      case _                       => BigInt(0)
    }

  private def outputTypeCost(tpe: __Type): BigInt = {
    val inner = tpe.innerType
    if (isAbstractType(inner)) {
      val concrete = inner.name.toList.flatMap(name => possibleTypesByName.getOrElse(name, Set.empty))
      concrete
        .map(name => costMetadata.types.getOrElse(name, BigInt(1)))
        .reduceOption(_ max _)
        .getOrElse(BigInt(1))
    } else namedTypeCost(inner)
  }

  private def namedTypeCost(tpe: __Type): BigInt =
    costMetadata.types.getOrElse(
      tpe.name.getOrElse(""),
      tpe.kind match {
        case __TypeKind.SCALAR | __TypeKind.ENUM => BigInt(0)
        case _                                   => BigInt(1)
      }
    )

  private def bounded(value: BigInt): Long =
    if (value > BigInt(Long.MaxValue)) Long.MaxValue
    else if (value < 0) 0L
    else value.longValue
}

private object OperationCost {
  // Field and argument costs are charged once; return-type costs are charged per result.
  private final case class FieldCostParts(oneTime: BigInt, perResult: BigInt) {
    def total(size: BigInt, nested: BigInt): BigInt = oneTime + size * (perResult + nested)
  }

  private final case class SizedPath(path: Vector[String], size: BigInt)

  private val Unsized: (BigInt, List[SizedPath]) = (BigInt(1), Nil)
}
