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

private[composition] final class CostCompilation private (subgraph: PreparedSubgraph) {
  import CostCompilation._

  private val types = subgraph.rootType.types

  def compile: Either[List[String], CostMetadata] = {
    val costs     = subgraph.applications(FederationDirective.Cost).flatMap { application =>
      application.directive.arguments.get("weight").collect { case weight: IntValue =>
        cost(application.coordinate, weight.toBigInt)
      }
    }
    val listSizes = subgraph.applications(FederationDirective.ListSize).flatMap {
      case FederationApplication(FieldCoordinate(typeName, fieldName), _, directive) =>
        types.get(typeName).flatMap(fieldDefinition(_, fieldName)).map(listSize(directive, typeName, _))
      case _                                                                         => None
    }
    validateAll(costs ::: listSizes).map(merge)
  }

  private def cost(coordinate: Coordinate, weight: BigInt): Either[String, CostMetadata] =
    (coordinate match {
      case TypeCoordinate(typeName, _)                                                                => Right(CostMetadata(types = Map(typeName -> weight)))
      case FieldCoordinate(typeName, _) if types.get(typeName).exists(_.kind == __TypeKind.INTERFACE) =>
        Left("@cost cannot be applied to an interface field.")
      case _                                                                                          => Right(CostMetadata(weights = Map(coordinate -> weight)))
    }).left.map(error => s"[${subgraph.name}] Invalid Federation @cost application at '${coordinate.display}': $error")

  private def listSize(
    directive: Directive,
    typeName: String,
    field: __Field
  ): Either[String, CostMetadata] = {
    val arguments = directive.arguments
    val result    = for {
      assumedSize  <- assumedSize(arguments)
      slicingPaths <- traverseEither(strings(arguments, "slicingArguments"))(slicingPath)
      slicing      <- traverseEither(slicingPaths)(slicingArgument(field, _))
      sizedPaths   <- traverseEither(strings(arguments, "sizedFields"))(sizedFieldPaths).map(_.flatten)
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
    } yield CostMetadata(listSizes =
      Map(
        SourceField(subgraph.name, typeName, field.name) -> ListSize(
          assumedSize,
          slicing,
          sizedPaths,
          !arguments.get("requireOneSlicingArgument").contains(BooleanValue(false))
        )
      )
    )
    result.left.map(error =>
      s"[${subgraph.name}] Invalid Federation @listSize application at '$typeName.${field.name}': $error"
    )
  }

  private def slicingArgument(field: __Field, path: ::[String]): Either[String, SlicingArgument] =
    field.allArgs
      .find(_.name == path.head)
      .flatMap { argument =>
        path.tail
          .foldLeft(Option(argument._type)) { (current, name) =>
            current.map(nullableType).flatMap { tpe =>
              if (tpe.kind == __TypeKind.LIST) None
              else inputFieldDefinition(tpe, name).map(_._type)
            }
          }
          .map(nullableType)
          .filter(tpe => tpe.kind == __TypeKind.LIST || (tpe.kind == __TypeKind.SCALAR && tpe.name.contains("Int")))
          .map(tpe => SlicingArgument(path, argument.parsedDefaultValue, tpe.isList))
      }
      .toRight(s"slicing argument '${path.mkString(".")}' must resolve to an Int or list argument.")

  private def sizedPathType(field: __Field, path: Vector[String]): Option[__Type] =
    path.foldLeft(Option(field._type)) { (current, name) =>
      current.flatMap(tpe => fieldDefinition(tpe.innerType, name).map(_._type))
    }
}

private[composition] object CostCompilation {

  def compile(subgraph: PreparedSubgraph): Either[List[String], CostMetadata] =
    new CostCompilation(subgraph).compile

  def merge(values: List[CostMetadata]): CostMetadata =
    CostMetadata(
      maximum(values.flatMap(_.types)),
      maximum(values.flatMap(_.weights)),
      values.flatMap(_.listSizes).toMap
    )

  private def maximum[K](values: Iterable[(K, BigInt)]): Map[K, BigInt] =
    values.groupMapReduce(_._1)(_._2)(_ max _)

  private def assumedSize(arguments: Map[String, InputValue]): Either[String, Option[BigInt]] =
    arguments.get("assumedSize").collect { case value: IntValue => value.toBigInt } match {
      case Some(value) if value < 0 => Left("the 'assumedSize' argument must not be negative.")
      case value                    => Right(value)
    }

  private def strings(arguments: Map[String, InputValue], name: String): List[String] =
    arguments.get(name).toList.flatMap(coercedList).collect { case StringValue(value) => value }

  private def slicingPath(value: String): Either[String, ::[String]] =
    value.split("\\.", -1).toList match {
      case path @ ::(_, _) if path.forall(_.nonEmpty) => Right(path)
      case _                                          => Left(s"slicing argument '$value' is not a valid path.")
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
