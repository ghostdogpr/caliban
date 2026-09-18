package caliban.gateway.internal.composition

import caliban.InputValue
import caliban.gateway._
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.composition.SchemaComposer.FederationDirectiveNames
import caliban.introspection.adt._
import caliban.parsing.adt.{ Directive, Selection }
import caliban.Value.{ BooleanValue, IntValue, StringValue }

import scala.collection.compat._

private[composition] final class CostCompilation private (
  subgraph: PreparedSubgraph,
  names: FederationDirectiveNames
) {
  import CostCompilation._

  def compile: Either[List[String], CostMetadata] = {
    val types     = subgraph.rootType.types.toList.sortBy(_._1).map { case (sourceName, tpe) =>
      subgraph.rootNames.composed(sourceName) -> tpe
    }
    val costs     = types.flatMap { case (typeName, tpe) => typeCosts(typeName, tpe) }
    val listSizes = types.flatMap { case (typeName, tpe) => typeListSizes(typeName, tpe) }

    (validateAll(costs), validateAll(listSizes)) match {
      case (Right(entries), Right(sizes)) =>
        Right(
          CostMetadata(
            maximum(entries.collect { case TypeCost(name, weight) => name -> weight }),
            maximum(entries.collect { case FieldCost(parent, name, weight) => TypeField(parent, name) -> weight }),
            maximum(entries.collect { case ArgumentCost(parent, field, name, weight) =>
              FieldArgument(parent, field, name) -> weight
            }),
            maximum(entries.collect { case InputFieldCost(parent, name, weight) =>
              TypeField(parent, name) -> weight
            }),
            sizes.toMap
          )
        )
      case (entries, sizes)               => Left(entries.left.getOrElse(Nil) ::: sizes.left.getOrElse(Nil))
    }
  }

  private def typeCosts(typeName: String, tpe: __Type): List[Either[String, CostEntry]] = {
    val supportedType   = tpe.kind == __TypeKind.OBJECT || tpe.kind == __TypeKind.SCALAR || tpe.kind == __TypeKind.ENUM
    val typeCost        = costApplications(
      tpe.directives,
      typeName,
      TypeCost(typeName, _),
      if (supportedType) None else Some("@cost is not supported at this type location.")
    )
    val fieldCosts      = tpe.allFields.flatMap { field =>
      costApplications(
        field.directives,
        s"$typeName.${field.name}",
        FieldCost(typeName, field.name, _),
        if (tpe.kind == __TypeKind.INTERFACE) Some("@cost cannot be applied to an interface field.") else None
      ) ::: field.allArgs.flatMap(argument =>
        costApplications(
          argument.directives,
          s"$typeName.${field.name}(${argument.name}:)",
          ArgumentCost(typeName, field.name, argument.name, _)
        )
      )
    }
    val inputFieldCosts = tpe.allInputFields.flatMap(field =>
      costApplications(field.directives, s"$typeName.${field.name}", InputFieldCost(typeName, field.name, _))
    )
    typeCost ::: fieldCosts ::: inputFieldCosts
  }

  private def costApplications(
    directives: Option[List[Directive]],
    coordinate: String,
    entry: Long => CostEntry,
    locationError: Option[String] = None
  ): List[Either[String, CostEntry]] =
    directives.getOrElse(Nil).filter(directive => names.cost.contains(directive.name)).map { directive =>
      val prefix = s"[${subgraph.name}] Invalid Federation @cost application at '$coordinate'"
      (locationError, directive.arguments.get("weight")) match {
        case (Some(error), _)                                 => Left(s"$prefix: $error")
        case _ if directive.arguments.keySet != Set("weight") =>
          Left(s"$prefix: exactly one 'weight' argument is required.")
        case (_, Some(weight: IntValue))                      => Right(entry(weight.toBigInt.longValue))
        case _                                                => Left(s"$prefix: the 'weight' argument must be an integer.")
      }
    }

  private def typeListSizes(typeName: String, tpe: __Type): List[Either[String, (SourceField, ListSize)]] = {
    def listSizeDirectives(directives: Option[List[Directive]]): List[Directive] =
      directives.getOrElse(Nil).filter(directive => names.listSize.contains(directive.name))

    val fieldListSizes =
      tpe.allFields.flatMap(field => listSizeDirectives(field.directives).map(listSize(_, typeName, field)))
    val misplaced      =
      listSizeDirectives(tpe.directives).map(_ => typeName) :::
        tpe.allFields.flatMap(field =>
          field.allArgs.flatMap(argument =>
            listSizeDirectives(argument.directives).map(_ => s"$typeName.${field.name}(${argument.name}:)")
          )
        ) :::
        tpe.allInputFields.flatMap(field => listSizeDirectives(field.directives).map(_ => s"$typeName.${field.name}"))
    fieldListSizes ::: misplaced.map(coordinate =>
      Left(
        s"[${subgraph.name}] Invalid Federation @listSize application at '$coordinate': @listSize is only supported on fields."
      )
    )
  }

  private def listSize(
    directive: Directive,
    typeName: String,
    field: __Field
  ): Either[String, (SourceField, ListSize)] = {
    val arguments = directive.arguments
    val result    =
      if (!arguments.keySet.subsetOf(ListSizeArguments)) Left("unsupported argument.")
      else
        for {
          assumedSize  <- assumedSize(arguments)
          slicing      <- stringList(arguments, "slicingArguments")
          sized        <- stringList(arguments, "sizedFields")
          requireOne   <- requireOneSlicingArgument(arguments)
          slicingPaths <- traverseEither(slicing)(slicingPath)
          _            <- traverseEither(slicingPaths)(validateSlicingPath(field, _))
          sizedPaths   <- traverseEither(sized)(sizedFieldPaths).map(_.flatten)
          _            <- Either.cond(
                            field._type.isList || sizedPaths.nonEmpty,
                            (),
                            "the field must return a list or define 'sizedFields'."
                          )
          _            <- traverseEither(sizedPaths)(path =>
                            Either.cond(
                              sizedPathType(field, path).exists(_.isList),
                              (),
                              s"sized field '${path.mkString(".")}' must exist and return a list."
                            )
                          )
        } yield SourceField(subgraph.name, typeName, field.name) -> ListSize(
          assumedSize,
          slicingPaths.map(slicingArgument(field, _)),
          sizedPaths,
          requireOne
        )
    result.left.map(error =>
      s"[${subgraph.name}] Invalid Federation @listSize application at '$typeName.${field.name}': $error"
    )
  }

  private def validateSlicingPath(field: __Field, path: Vector[String]): Either[String, Unit] = {
    val valid = inputPathType(field, path).map(nullableType).exists { tpe =>
      tpe.kind == __TypeKind.LIST || (tpe.kind == __TypeKind.SCALAR && tpe.name.contains("Int"))
    }
    Either.cond(valid, (), s"slicing argument '${path.mkString(".")}' must resolve to an Int or list argument.")
  }

  private def slicingArgument(field: __Field, path: Vector[String]): SlicingArgument = {
    val default = path.headOption.flatMap(name => field.allArgs.find(_.name == name)).flatMap(_.parsedDefaultValue)
    SlicingArgument(path, default, inputPathType(field, path).exists(_.isList))
  }

  private def inputPathType(field: __Field, path: Vector[String]): Option[__Type] =
    path.headOption.flatMap(name => field.allArgs.find(_.name == name)).flatMap { argument =>
      path.tail.foldLeft(Option(argument._type)) { (current, name) =>
        current.map(nullableType).flatMap { tpe =>
          if (tpe.kind == __TypeKind.LIST) None
          else resolvedType(tpe).allInputFields.find(_.name == name).map(_._type)
        }
      }
    }

  private def sizedPathType(field: __Field, path: Vector[String]): Option[__Type] =
    path.foldLeft(Option(field._type)) { (current, name) =>
      current.flatMap(tpe => Option(resolvedType(tpe).getFieldOrNull(name)).map(_._type))
    }

  private def resolvedType(tpe: __Type): __Type = {
    val inner = tpe.innerType
    inner.name.flatMap(subgraph.rootType.types.get).getOrElse(inner)
  }
}

private[composition] object CostCompilation {

  def compile(subgraph: PreparedSubgraph, names: FederationDirectiveNames): Either[List[String], CostMetadata] =
    new CostCompilation(subgraph, names).compile

  def merge(values: List[CostMetadata]): CostMetadata =
    CostMetadata(
      maximum(values.flatMap(_.types)),
      maximum(values.flatMap(_.fields)),
      maximum(values.flatMap(_.arguments)),
      maximum(values.flatMap(_.inputFields)),
      values.flatMap(_.listSizes).toMap
    )

  private val ListSizeArguments = Set("assumedSize", "slicingArguments", "sizedFields", "requireOneSlicingArgument")

  private sealed trait CostEntry
  private final case class TypeCost(name: String, weight: Long)                                    extends CostEntry
  private final case class FieldCost(parent: String, name: String, weight: Long)                   extends CostEntry
  private final case class ArgumentCost(parent: String, field: String, name: String, weight: Long) extends CostEntry
  private final case class InputFieldCost(parent: String, name: String, weight: Long)              extends CostEntry

  private def maximum[K](values: Iterable[(K, Long)]): Map[K, Long] =
    values.groupMapReduce(_._1)(_._2)(_ max _)

  private def assumedSize(arguments: Map[String, InputValue]): Either[String, Option[Long]] =
    arguments.get("assumedSize") match {
      case None                                         => Right(None)
      case Some(value: IntValue) if value.toBigInt >= 0 => Right(Some(value.toBigInt.longValue))
      case Some(_: IntValue)                            => Left("the 'assumedSize' argument must not be negative.")
      case _                                            => Left("the 'assumedSize' argument must be an integer.")
    }

  private def requireOneSlicingArgument(arguments: Map[String, InputValue]): Either[String, Boolean] =
    arguments.get("requireOneSlicingArgument") match {
      case None                      => Right(true)
      case Some(BooleanValue(value)) => Right(value)
      case _                         => Left("the 'requireOneSlicingArgument' argument must be a boolean.")
    }

  private def stringList(arguments: Map[String, InputValue], name: String): Either[String, List[String]] = {
    val error = s"the '$name' argument must be a list of strings."
    arguments.get(name) match {
      case None                               => Right(Nil)
      case Some(StringValue(value))           => Right(value :: Nil)
      case Some(InputValue.ListValue(values)) => stringValues(values).toRight(error)
      case _                                  => Left(error)
    }
  }

  private def slicingPath(value: String): Either[String, Vector[String]] = {
    val path = value.split("\\.", -1).toVector
    Either.cond(path.forall(_.nonEmpty), path, s"slicing argument '$value' is not a valid path.")
  }

  private def sizedFieldPaths(value: String): Either[String, List[Vector[String]]] = {
    val invalidPath = s"sized field '$value' is not a valid field path."
    for {
      parsed <- parseFieldSet(value).toRight(invalidPath)
      paths  <- selectionPaths(parsed, Vector.empty).toRight(invalidPath)
      _      <- Either.cond(hasNoSiblingLeaves(parsed), (), s"sized field '$value' must not select sibling leaf fields.")
    } yield paths
  }

  private def selectionPaths(selections: List[Selection], prefix: Vector[String]): Option[List[Vector[String]]] =
    traverseOption(selections) {
      case Selection.Field(None, name, arguments, directives, children, _) if arguments.isEmpty && directives.isEmpty =>
        if (children.isEmpty) Some(List(prefix :+ name)) else selectionPaths(children, prefix :+ name)
      case _                                                                                                          => None
    }.map(_.flatten)

  private def hasNoSiblingLeaves(selections: List[Selection]): Boolean =
    selections.count {
      case Selection.Field(_, _, _, _, children, _) => children.isEmpty
      case _                                        => false
    } <= 1 && selections.forall {
      case Selection.Field(_, _, _, _, children, _) => children.isEmpty || hasNoSiblingLeaves(children)
      case _                                        => true
    }
}
