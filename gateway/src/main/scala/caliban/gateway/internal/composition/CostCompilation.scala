package caliban.gateway.internal.composition

import caliban.InputValue
import caliban.gateway._
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.composition.DirectiveComposition._
import caliban.gateway.internal.composition.FederationCompilation._
import caliban.gateway.internal.composition.SchemaComposer.PreparedSubgraph
import caliban.introspection.adt._
import caliban.parsing.adt.Directive
import caliban.Value.{ BooleanValue, IntValue, StringValue }

import scala.collection.compat._

private[composition] final class CostCompilation private (
  subgraph: PreparedSubgraph,
  names: FederationDirectiveNames
) {
  import CostCompilation._

  private val types = subgraph.rootType.types.map { case (sourceName, tpe) =>
    subgraph.rootNames.composed(sourceName) -> tpe
  }

  def compile(applications: List[TypeSystemDirectiveApplication]): Either[List[String], CostMetadata] = {
    val costs     = applications.flatMap { application =>
      application.directives
        .filter(directive => names.cost.contains(directive.name))
        .map(cost(application.coordinate, _))
    }
    val listSizes = applications.flatMap { application =>
      application.directives.filter(directive => names.listSize.contains(directive.name)).flatMap { directive =>
        application.coordinate match {
          case FieldCoordinate(typeName, fieldName) =>
            types.get(typeName).flatMap(fieldDefinition(_, fieldName)).map(listSize(directive, typeName, _))
          case coordinate                           =>
            Some(
              Left(
                s"[${subgraph.name}] Invalid Federation @listSize application at '${coordinate.display}': @listSize is only supported on fields."
              )
            )
        }
      }
    }

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

  private def cost(coordinate: Coordinate, directive: Directive): Either[String, CostEntry] = {
    val entry: Either[String, BigInt => CostEntry] = coordinate match {
      case TypeCoordinate(typeName, location)
          if location == __DirectiveLocation.OBJECT || location == __DirectiveLocation.SCALAR ||
            location == __DirectiveLocation.ENUM =>
        Right(TypeCost(typeName, _))
      case FieldCoordinate(typeName, _) if types.get(typeName).exists(_.kind == __TypeKind.INTERFACE) =>
        Left("@cost cannot be applied to an interface field.")
      case FieldCoordinate(typeName, fieldName)                                                       =>
        Right(FieldCost(typeName, fieldName, _))
      case ArgumentCoordinate(typeName, fieldName, argumentName)                                      =>
        Right(ArgumentCost(typeName, fieldName, argumentName, _))
      case InputFieldCoordinate(typeName, fieldName)                                                  =>
        Right(InputFieldCost(typeName, fieldName, _))
      case _: TypeCoordinate                                                                          =>
        Left("@cost is not supported at this type location.")
      case _                                                                                          =>
        Left("@cost is not supported at this location.")
    }
    val prefix                                     = s"[${subgraph.name}] Invalid Federation @cost application at '${coordinate.display}'"
    (entry, directive.arguments.get("weight")) match {
      case (Left(error), _)                                 => Left(s"$prefix: $error")
      case _ if directive.arguments.keySet != Set("weight") =>
        Left(s"$prefix: exactly one 'weight' argument is required.")
      case (Right(entry), Some(weight: IntValue))           => Right(entry(weight.toBigInt))
      case _                                                => Left(s"$prefix: the 'weight' argument must be an integer.")
    }
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
          else inputFieldDefinition(resolvedType(tpe), name).map(_._type)
        }
      }
    }

  private def sizedPathType(field: __Field, path: Vector[String]): Option[__Type] =
    path.foldLeft(Option(field._type)) { (current, name) =>
      current.flatMap(tpe => fieldDefinition(resolvedType(tpe), name).map(_._type))
    }

  private def resolvedType(tpe: __Type): __Type = {
    val inner = tpe.innerType
    inner.name.flatMap(subgraph.rootType.types.get).getOrElse(inner)
  }
}

private[composition] object CostCompilation {

  def compile(
    subgraph: PreparedSubgraph,
    names: FederationDirectiveNames,
    applications: List[TypeSystemDirectiveApplication]
  ): Either[List[String], CostMetadata] =
    new CostCompilation(subgraph, names).compile(applications)

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
  private final case class TypeCost(name: String, weight: BigInt)                                    extends CostEntry
  private final case class FieldCost(parent: String, name: String, weight: BigInt)                   extends CostEntry
  private final case class ArgumentCost(parent: String, field: String, name: String, weight: BigInt) extends CostEntry
  private final case class InputFieldCost(parent: String, name: String, weight: BigInt)              extends CostEntry

  private def maximum[K](values: Iterable[(K, BigInt)]): Map[K, BigInt] =
    values.groupMapReduce(_._1)(_._2)(_ max _)

  private def assumedSize(arguments: Map[String, InputValue]): Either[String, Option[BigInt]] =
    arguments.get("assumedSize") match {
      case None                                         => Right(None)
      case Some(value: IntValue) if value.toBigInt >= 0 => Right(Some(value.toBigInt))
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
      fields <- plainFieldSet(parsed).toRight(invalidPath)
      _      <- Either.cond(hasNoSiblingLeaves(fields), (), s"sized field '$value' must not select sibling leaf fields.")
    } yield fieldPaths(fields, Vector.empty)
  }

  private def fieldPaths(fields: List[KeyField], prefix: Vector[String]): List[Vector[String]] =
    fields.flatMap { field =>
      if (field.children.isEmpty) List(prefix :+ field.name) else fieldPaths(field.children, prefix :+ field.name)
    }

  private def hasNoSiblingLeaves(fields: List[KeyField]): Boolean =
    fields.count(_.children.isEmpty) <= 1 && fields.forall(field =>
      field.children.isEmpty || hasNoSiblingLeaves(field.children)
    )
}
