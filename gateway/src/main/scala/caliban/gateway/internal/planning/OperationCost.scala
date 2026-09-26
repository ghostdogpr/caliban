package caliban.gateway.internal.planning

import caliban.InputValue
import caliban.execution.Field
import caliban.gateway._
import caliban.gateway.internal.composition.ComposedGraph._
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
    val hasListSizes = costMetadata.listSizes.nonEmpty
    plan.passthroughSubgraph match {
      case Some(source) =>
        val fields = plan.fields.map(field => field.copy(fields = collectedFields(field)))
        val error  = if (hasListSizes) validateListSizes(fields, source) else None
        error.toLeft(bounded(operationBase(plan.operationType) + fieldsCost(fields, source)))
      case None         =>
        val error =
          if (!hasListSizes) None
          else
            firstError(
              plan.roots.iterator.map(fetch => validateListSizes(fetch.downstream, fetch.source)) ++
                plan.entities.iterator.map(fetch => validateListSizes(fetch.fields, fetch.source))
            )
        error.toLeft {
          val multipliers = if (hasListSizes) representationMultipliers(plan) else NoMultipliers
          val rootCost    = plan.roots.foldLeft(BigInt(0)) { (total, fetch) =>
            total + operationBase(plan.operationType) + fieldsCost(fetch.downstream, fetch.source)
          }
          val entityCost  = plan.entities
            .groupBy(fetch => fetch.root -> fetch.mergePath)
            .values
            .foldLeft(BigInt(0))((total, fetches) => total + entityFetchesCost(fetches, multipliers))
          bounded(rootCost + entityCost)
        }
    }
  }

  private def collectedFields(parent: Field): List[Field] =
    if (parent.allFieldsUniqueNameAndCondition)
      parent.fields.map(field => field.copy(fields = collectedFields(field)))
    else {
      val name          = parent.fieldType.innerType.name.getOrElse("")
      val possibleTypes = possibleTypesByName.getOrElse(name, Set(name))
      val fields        = mutable.LinkedHashMap.empty[Field, Set[String]]
      possibleTypes.toList.sorted.foreach { runtime =>
        parent.collectFields(runtime).foreach { field =>
          val shared = field.copy(_condition = None)
          fields.update(shared, fields.getOrElse(shared, Set.empty) + runtime)
        }
      }
      fields.iterator.map { case (field, members) =>
        val condition = if (members == possibleTypes) None else Some(members)
        field.copy(fields = collectedFields(field), _condition = condition)
      }.toList
    }

  private def operationBase(operation: OperationType): BigInt =
    if (operation == OperationType.Mutation) BigInt(10) else BigInt(0)

  private def fieldsCost(fields: List[Field], source: String, paths: List[SizedPath] = Nil): BigInt =
    conditionalCost(
      fields.map(field => field._condition -> fieldCost(field, source, paths)),
      (entry: (Option[Set[String]], BigInt)) => entry._1
    )((entry, _) => entry._2)

  private def entityFetchCost(
    fetch: EntityFetch,
    multipliers: FetchMultipliers,
    runtimeType: Option[String]
  ): BigInt = {
    val entity     = types.get(fetch.entityType).fold(BigInt(1))(outputTypeCost).max(BigInt(0))
    val fields     =
      runtimeType.fold(fetch.fields)(runtime => fetch.fields.filter(_._condition.forall(_.contains(runtime))))
    val selections = fieldsCost(fields, fetch.source, multipliers.sizedFields.getOrElse(fetch.mergePath, Nil))
    multipliers.representations.getOrElse(fetch.mergePath, BigInt(1)) * (entity + selections)
  }

  private def entityFetchesCost(fetches: List[EntityFetch], multipliers: FetchMultipliers): BigInt =
    conditionalCost(
      fetches,
      (fetch: EntityFetch) => Some(fetch.fields.flatMap(_._condition.toList.flatten).toSet).filter(_.nonEmpty)
    )(entityFetchCost(_, multipliers, _))

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

  private def fieldCost(field: Field, source: String, paths: List[SizedPath]): BigInt = {
    val (size, nestedPaths) = sizing(field, source, paths)
    val nested              = fieldsCost(field.fields, source, nestedPaths)
    maximumFieldCost(field)(_.total(size, nested))
  }

  private def sizing(field: Field, source: String, paths: List[SizedPath]): (BigInt, List[SizedPath]) = {
    val definitions            = fieldListSizes(field, source)
    val (activated, remaining) = matchingSizedPaths(field.name, paths).partition(_.path.isEmpty)
    val direct                 = definitions.filter(_.sizedFields.isEmpty).map(resolvedListSize(field, _))
    val declared               = definitions.flatMap { definition =>
      val size = resolvedListSize(field, definition)
      definition.sizedFields.map(SizedPath(_, size))
    }
    activated.map(_.size).reduceOption(_ max _).orElse(direct.reduceOption(_ max _)).getOrElse(BigInt(1)) ->
      preferSizedPaths(declared, remaining)
  }

  // A nearer @listSize replaces an inherited size for the same path.
  private def preferSizedPaths(primary: List[SizedPath], fallback: List[SizedPath]): List[SizedPath] = {
    val preferred      = maximumSizedPaths(primary)
    val preferredPaths = preferred.map(_.path).toSet
    preferred ::: maximumSizedPaths(fallback.filterNot(value => preferredPaths.contains(value.path)))
  }

  private def maximumSizedPaths(values: List[SizedPath]): List[SizedPath] =
    values
      .groupBy(_.path)
      .map { case (path, entries) => SizedPath(path, entries.map(_.size).max) }
      .toList

  private def fieldListSizes(field: Field, source: String): List[ListSize] =
    if (costMetadata.listSizes.isEmpty) Nil
    else {
      val parent   = innerParentTypeName(field)
      val direct   = costMetadata.listSizes.get(SourceField(source, parent, field.name)).toList
      val concrete =
        implementingParents(field).toList.flatMap(name =>
          costMetadata.listSizes.get(SourceField(source, name, field.name))
        )
      (direct ::: concrete).distinct
    }

  private def representationMultipliers(plan: OperationPlan): FetchMultipliers = {
    // Merge paths use response aliases; pending sized paths use schema field names.
    var representations = Map.empty[Vector[String], BigInt]
    var pendingByPath   = Map.empty[Vector[String], List[SizedPath]]

    def record(path: Vector[String], multiplier: BigInt, pending: List[SizedPath]): Unit = {
      representations = representations.updated(path, representations.get(path).fold(multiplier)(_ max multiplier))
      pendingByPath = pendingByPath.updated(path, maximumSizedPaths(pendingByPath.getOrElse(path, Nil) ::: pending))
    }

    def collect(
      fields: List[Field],
      source: String,
      basePath: Vector[String],
      inherited: BigInt,
      pending: List[SizedPath]
    ): Unit =
      fields.foreach { field =>
        val path                  = basePath :+ field.aliasedName
        val (size, nestedPending) = sizing(field, source, pending)
        val multiplier            = inherited * size
        record(path, multiplier, nestedPending)
        collect(field.fields, source, path, multiplier, nestedPending)
      }

    plan.roots.foreach(fetch => collect(fetch.downstream, fetch.source, Vector.empty, BigInt(1), Nil))
    plan.entities.foreach { fetch =>
      val inherited = representations.getOrElse(fetch.mergePath, BigInt(1))
      collect(fetch.fields, fetch.source, fetch.mergePath, inherited, pendingByPath.getOrElse(fetch.mergePath, Nil))
    }
    new FetchMultipliers(representations, pendingByPath)
  }

  private def matchingSizedPaths(name: String, paths: List[SizedPath]): List[SizedPath] =
    paths.filter(_.path.headOption.contains(name)).map(value => SizedPath(value.path.drop(1), value.size))

  private def validateListSizes(fields: List[Field], source: String): Option[String] =
    firstError(fields.iterator.map(validateListSize(_, source)))

  private def validateListSize(field: Field, source: String): Option[String] = {
    val invalid = fieldListSizes(field, source).exists { listSize =>
      listSize.requireOneSlicingArgument && listSize.slicingArguments.nonEmpty &&
      listSize.slicingArguments.count(argument => slicingValue(field, argument).nonEmpty) != 1
    }
    if (invalid)
      Some(s"Exactly one slicing argument must be supplied for field '${innerParentTypeName(field)}.${field.name}'.")
    else validateListSizes(field.fields, source)
  }

  private def firstError(errors: Iterator[Option[String]]): Option[String] =
    errors.collectFirst { case Some(error) => error }

  private def resolvedListSize(field: Field, listSize: ListSize): BigInt =
    listSize.slicingArguments
      .flatMap(argument => slicingValue(field, argument))
      .reduceOption(_ max _)
      .orElse(listSize.assumedSize)
      .getOrElse(BigInt(1))

  private def slicingValue(field: Field, argument: SlicingArgument): Option[BigInt] = {
    val path = argument.path

    def nested(value: InputValue, remaining: Vector[String]): Option[InputValue] =
      remaining.headOption match {
        case None       => Some(value)
        case Some(name) =>
          value match {
            case InputValue.ObjectValue(values) => values.get(name).flatMap(nested(_, remaining.tail))
            case _                              => None
          }
      }

    path.headOption
      .flatMap(name => field.arguments.get(name).orElse(argument.defaultValue))
      .flatMap(nested(_, path.drop(1)))
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
    val oneTime = costMetadata.fields.get(TypeField(parent, field.name)).getOrElse(BigInt(0)) +
      argumentCost(parent, field, definitions)
    FieldCostParts(oneTime.max(BigInt(0)), outputTypeCost(fieldType).max(BigInt(0)))
  }

  private def argumentCost(parent: String, field: Field, arguments: List[__InputValue]): BigInt =
    arguments.foldLeft(BigInt(0)) { (total, argument) =>
      field.arguments.get(argument.name).orElse(argument.parsedDefaultValue) match {
        case Some(value) =>
          val base = costMetadata.arguments
            .get(FieldArgument(parent, field.name, argument.name))
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
                  val base = costMetadata.inputFields
                    .get(TypeField(tpe.name.getOrElse(""), field.name))
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

  private final class FetchMultipliers(
    val representations: Map[Vector[String], BigInt],
    val sizedFields: Map[Vector[String], List[SizedPath]]
  )

  private val NoMultipliers = new FetchMultipliers(Map.empty, Map.empty)
}
