package caliban.gateway.internal.composition

import caliban.gateway._
import caliban.gateway.internal.composition.SchemaComposer.PreparedSubgraph
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.introspection.adt._

private[composition] final class LookupCompilation private (subgraph: PreparedSubgraph, lookup: Lookup) {
  import LookupCompilation._

  private val prefix      = s"[${subgraph.name}]"
  private val rootName    = subgraph.rootType.queryType.name.getOrElse("Query")
  private val targetType  = subgraph.rootType.types.get(lookup.typeName)
  private val sourceField = subgraph.rootType.queryType.allFields.find(_.name == lookup.field)
  private val keys        = targetType.toList
    .flatMap(target => lookup.keyFields.flatMap(name => target.allFields.find(_.name == name).map(name -> _)))
    .toMap

  def compile: Either[List[String], LookupOperation.GraphQLQuery] = {
    val targetDiagnostics = targetTypeDiagnostics ::: keyDiagnostics
    sourceField match {
      case None        => Left(targetDiagnostics :+ s"$prefix Lookup field '$rootName.${lookup.field}' does not exist.")
      case Some(field) =>
        val arguments   = compileArguments(field)
        val diagnostics = targetDiagnostics ::: shapeDiagnostics(field) ::: arguments.left.getOrElse(Nil) :::
          correlationDiagnostics(field)
        arguments match {
          case Right(values) if diagnostics.isEmpty => Right(LookupOperation.GraphQLQuery(lookup.field, values, result))
          case _                                    => Left(diagnostics)
        }
    }
  }

  private def result: LookupResult =
    lookup match {
      case _: Lookup.Single    => LookupResult.Single
      case byKey: Lookup.ByKey => LookupResult.ByKey(byKey.correlation)
    }

  private def targetTypeDiagnostics: List[String] =
    targetType match {
      case None                                             =>
        List(s"$prefix Lookup target type '${lookup.typeName}' does not exist.")
      case Some(target) if target.kind != __TypeKind.OBJECT =>
        List(s"$prefix Lookup target type '${lookup.typeName}' must be an object type.")
      case Some(_)                                          => Nil
    }

  private def keyDiagnostics: List[String] = {
    val missing  =
      check(lookup.keyFields.nonEmpty, s"$prefix Lookup for '${lookup.typeName}' must declare at least one key field.")
    val repeated = duplicates(lookup.keyFields).map(name =>
      s"$prefix Lookup key field '${lookup.typeName}.$name' is declared more than once."
    )
    val invalid  =
      if (targetType.isEmpty) Nil
      else
        lookup.keyFields.flatMap { name =>
          keys.get(name).map(field => nullableType(field._type).kind) match {
            case None                                                               =>
              List(s"$prefix Lookup key field '${lookup.typeName}.$name' does not exist.")
            case Some(kind) if kind == __TypeKind.SCALAR || kind == __TypeKind.ENUM => Nil
            case Some(_)                                                            =>
              List(s"$prefix Lookup key field '${lookup.typeName}.$name' must be a scalar or enum.")
          }
        }
    missing ::: repeated ::: invalid
  }

  private def shapeDiagnostics(field: __Field): List[String] = {
    def isTarget(tpe: __Type): Boolean = tpe.kind != __TypeKind.LIST && tpe.name.contains(lookup.typeName)

    val resultType     = nullableType(field._type)
    val (valid, shape) = lookup match {
      case _: Lookup.Single => (isTarget(resultType), s"'${lookup.typeName}'")
      case _: Lookup.ByKey  =>
        val isTargetList = resultType.kind == __TypeKind.LIST && resultType.ofType.map(nullableType).exists(isTarget)
        (isTargetList, s"a list of '${lookup.typeName}'")
    }
    check(valid, s"$prefix Lookup field '$rootName.${lookup.field}' must return $shape.")
  }

  private def compileArguments(field: __Field): Either[List[String], Map[String, LookupArgument]] = {
    val definitions = field.allArgs.map(argument => argument.name -> argument).toMap
    val mapped      = lookup.arguments.map(_._1)
    val unknown     = mapped
      .filterNot(definitions.contains)
      .map(name => s"$prefix Lookup field '$rootName.${lookup.field}' has no argument '$name'.")
    val repeated    =
      duplicates(mapped).map(name => s"$prefix Lookup argument '${lookup.field}.$name' is mapped more than once.")
    val missing     = field.allArgs.collect {
      case argument if !mapped.contains(argument.name) && isRequiredInput(argument) =>
        s"$prefix Required lookup argument '${lookup.field}.${argument.name}' has no mapping."
    }
    val mappings    = validateMappings(lookup.arguments.flatMap { case (name, mapping) =>
      definitions.get(name).toList.map(argument => name -> compileArgument(name, mapping, argument._type))
    })
    val hasBatch    = lookup.arguments.exists { case (_, mapping) => containsBatch(mapping) }
    val batch       = lookup match {
      case _: Lookup.Single if hasBatch                                                                 =>
        List(s"$prefix Single lookup argument mappings cannot contain a batch mapping.")
      case _: Lookup.ByKey if !hasBatch                                                                 =>
        List(s"$prefix List lookup argument mappings must contain a batch mapping.")
      case _: Lookup.ByKey if lookup.arguments.exists { case (_, mapping) => keyOutsideBatch(mapping) } =>
        List(s"$prefix List lookup key mappings must be nested inside a batch mapping.")
      case _                                                                                            => Nil
    }
    val mappedKeys  = lookup.arguments.flatMap { case (_, mapping) => argumentKeys(mapping) }.toSet
    val keyCoverage =
      check(
        mappedKeys == lookup.keyFields.toSet,
        s"$prefix Lookup argument mappings must use every declared key field."
      )

    val diagnostics = unknown ::: repeated ::: missing ::: mappings.left.getOrElse(Nil) ::: batch ::: keyCoverage
    mappings match {
      case Right(values) if diagnostics.isEmpty => Right(values.toMap)
      case _                                    => Left(diagnostics)
    }
  }

  private def compileArgument(
    path: String,
    mapping: Lookup.Argument,
    expected: __Type
  ): Either[List[String], LookupArgument] = {
    val valueType = nullableType(expected)
    mapping match {
      case Lookup.Argument.Key(field)                                                    =>
        keys.get(field) match {
          case None                                                             =>
            Left(List(s"$prefix Lookup argument '$path' references undeclared key field '$field'."))
          case Some(keyField) if compatibleValueType(keyField._type, valueType) =>
            Right(LookupArgument.Key(field, valueType))
          case Some(keyField)                                                   =>
            Left(List(s"$prefix Lookup argument '$path' is incompatible with key field '${keyField.name}'."))
        }
      case Lookup.Argument.ObjectMapping(_) if valueType.kind != __TypeKind.INPUT_OBJECT =>
        Left(List(s"$prefix Lookup argument '$path' maps an object into a non-input-object value."))
      case Lookup.Argument.ObjectMapping(fields)                                         =>
        compileObjectMapping(path, fields, valueType)
      case Lookup.Argument.Batch(value) if containsBatch(value)                          =>
        Left(List(s"$prefix Lookup argument '$path' cannot nest a batch mapping."))
      case Lookup.Argument.Batch(_) if !valueType.isList                                 =>
        Left(List(s"$prefix Lookup argument '$path' maps a batch into a non-list value."))
      case Lookup.Argument.Batch(value)                                                  =>
        compileArgument(path, value, valueType.ofType.map(nullableType).getOrElse(valueType))
          .map(LookupArgument.Batch(_))
    }
  }

  private def compileObjectMapping(
    path: String,
    fields: List[(String, Lookup.Argument)],
    inputType: __Type
  ): Either[List[String], LookupArgument] = {
    val inputFields = inputType.allInputFields.map(field => field.name -> field).toMap
    val mapped      = fields.map(_._1)
    val repeated    = duplicates(mapped).map(name => s"$prefix Lookup argument '$path.$name' is mapped more than once.")
    val unknown     = mapped.collect {
      case name if !inputFields.contains(name) => s"$prefix Lookup input field '$path.$name' does not exist."
    }
    val missing     = inputType.allInputFields.collect {
      case input if !mapped.contains(input.name) && isRequiredInput(input) =>
        s"$prefix Required lookup input field '$path.${input.name}' has no mapping."
    }
    val mappings    = validateMappings(fields.flatMap { case (name, value) =>
      inputFields.get(name).toList.map(input => name -> compileArgument(s"$path.$name", value, input._type))
    })
    val diagnostics = repeated ::: unknown ::: missing ::: mappings.left.getOrElse(Nil)
    mappings match {
      case Right(values) if diagnostics.isEmpty => Right(LookupArgument.ObjectMapping(values))
      case _                                    => Left(diagnostics)
    }
  }

  private def correlationDiagnostics(field: __Field): List[String] =
    (lookup, targetType) match {
      case (byKey: Lookup.ByKey, Some(target)) =>
        val nullability = check(
          nullableType(field._type).ofType.exists(!_.isNullable),
          s"$prefix By-key lookup field '$rootName.${lookup.field}' must return non-null items."
        )
        val coverage    = check(
          byKey.correlation.values.toList.sorted == lookup.keyFields.sorted,
          s"$prefix By-key lookup correlation must map every declared key field exactly once."
        )
        val values      = byKey.correlation.toList.flatMap { case (responseField, keyField) =>
          (target.allFields.find(_.name == responseField), keys.get(keyField)) match {
            case (None, _)                                                                     =>
              List(s"$prefix Lookup correlation field '${lookup.typeName}.$responseField' does not exist.")
            case (_, None)                                                                     =>
              List(s"$prefix Lookup correlation references undeclared key field '$keyField'.")
            case (Some(response), Some(key)) if compatibleValueType(response._type, key._type) => Nil
            case _                                                                             =>
              List(
                s"$prefix Lookup correlation field '${lookup.typeName}.$responseField' is incompatible with key '$keyField'."
              )
          }
        }
        nullability ::: coverage ::: values
      case _                                   => Nil
    }
}

private[composition] object LookupCompilation {

  def compile(subgraph: PreparedSubgraph, lookup: Lookup): Either[List[String], LookupOperation.GraphQLQuery] =
    new LookupCompilation(subgraph, lookup).compile

  def declarationDiagnostics(subgraph: PreparedSubgraph): List[String] = {
    val federation = check(
      !subgraph.federation || subgraph.lookups.isEmpty,
      s"[${subgraph.name}] Ordinary GraphQL lookups cannot be declared on a Federation subgraph."
    )
    federation ::: duplicates(subgraph.lookups.map(_.typeName)).map(typeName =>
      s"[${subgraph.name}] More than one lookup is declared for type '$typeName'."
    )
  }

  private def validateMappings(
    results: List[(String, Either[List[String], LookupArgument])]
  ): Either[List[String], List[(String, LookupArgument)]] = {
    val errors = results.flatMap(_._2.left.getOrElse(Nil))
    if (errors.nonEmpty) Left(errors)
    else Right(results.collect { case (name, Right(value)) => name -> value })
  }

  private def compatibleValueType(left: __Type, right: __Type): Boolean = {
    val a = nullableType(left)
    val b = nullableType(right)
    a.kind == b.kind && a.name == b.name
  }

  private def containsBatch(argument: Lookup.Argument): Boolean =
    argument match {
      case _: Lookup.Argument.Key                => false
      case Lookup.Argument.ObjectMapping(fields) => fields.exists(value => containsBatch(value._2))
      case _: Lookup.Argument.Batch              => true
    }

  private def keyOutsideBatch(argument: Lookup.Argument): Boolean =
    argument match {
      case _: Lookup.Argument.Key                => true
      case Lookup.Argument.ObjectMapping(fields) => fields.exists(value => keyOutsideBatch(value._2))
      case _: Lookup.Argument.Batch              => false
    }

  private def argumentKeys(argument: Lookup.Argument): List[String] =
    argument match {
      case Lookup.Argument.Key(field)            => field :: Nil
      case Lookup.Argument.ObjectMapping(fields) => fields.flatMap(value => argumentKeys(value._2))
      case Lookup.Argument.Batch(value)          => argumentKeys(value)
    }
}
