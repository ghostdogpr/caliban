package caliban.gateway.internal.composition

import caliban.gateway._
import caliban.gateway.internal.composition.SchemaComposer.PreparedSubgraph
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.introspection.adt._

private[composition] final class LookupCompilation private (subgraph: PreparedSubgraph, lookup: Lookup) {
  private val prefix      = s"[${subgraph.name}]"
  private val targetType  = subgraph.rootType.types.get(lookup.typeName)
  private val sourceField = fieldDefinition(subgraph.rootType.queryType, lookup.field)
  private val keys        = lookup.keyFields.flatMap(name => targetType.flatMap(fieldDefinition(_, name)).map(name -> _)).toMap

  def compile: Either[List[String], LookupOperation.GraphQLQuery] = {
    val targetDiagnostics = targetTypeDiagnostics ::: keyDiagnostics
    sourceField match {
      case None        => Left(targetDiagnostics :+ s"$prefix Lookup field 'Query.${lookup.field}' does not exist.")
      case Some(field) =>
        val resultType = nullableType(field._type)
        val compiled   = lookup match {
          case Lookup.Single(_, _, _, arguments)             =>
            validated(
              shapeDiagnostics(isTarget(resultType), s"'${lookup.typeName}'"),
              compileArguments(field, arguments, arguments.flatMap(_._2.leaves))(compileKey)
            ).map(_ -> LookupResult.Single)
          case Lookup.ByKey(_, _, _, arguments, correlation) =>
            val isTargetList =
              resultType.kind == __TypeKind.LIST && resultType.ofType.map(nullableType).exists(isTarget)
            validated(
              shapeDiagnostics(isTargetList, s"a list of '${lookup.typeName}'") :::
                correlationDiagnostics(field, correlation),
              compileArguments(field, arguments, arguments.flatMap(_._2.leaves).flatMap(_.value.leaves))(compileBatch)
            ).map(_ -> LookupResult.ByKey(correlation.map(_.swap)))
        }
        validated(targetDiagnostics, compiled).map { case (arguments, result) =>
          LookupOperation.GraphQLQuery(lookup.field, arguments, result)
        }
    }
  }

  private def isTarget(tpe: __Type): Boolean = tpe.kind != __TypeKind.LIST && tpe.name.contains(lookup.typeName)

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

  private def shapeDiagnostics(valid: Boolean, shape: String): List[String] =
    check(valid, s"$prefix Lookup field 'Query.${lookup.field}' must return $shape.")

  private def compileArguments[A](
    field: __Field,
    arguments: List[(String, Lookup.Argument[A])],
    mappedKeys: List[Lookup.Key]
  )(
    leaf: (String, A, __Type) => Either[List[String], LookupArgument]
  ): Either[List[String], Map[String, LookupArgument]] = {
    val definitions = field.allArgs.map(argument => argument.name -> argument).toMap
    val mapped      = arguments.map(_._1)
    val unknown     = mapped
      .filterNot(definitions.contains)
      .map(name => s"$prefix Lookup field 'Query.${lookup.field}' has no argument '$name'.")
    val repeated    =
      duplicates(mapped).map(name => s"$prefix Lookup argument '${lookup.field}.$name' is mapped more than once.")
    val missing     = field.allArgs.collect {
      case argument if !mapped.contains(argument.name) && isRequiredInput(argument) =>
        s"$prefix Required lookup argument '${lookup.field}.${argument.name}' has no mapping."
    }
    val mappings    = collectErrors(arguments.flatMap { case (name, mapping) =>
      definitions.get(name).toList.map(argument => compileArgument(name, mapping, argument._type)(leaf).map(name -> _))
    })
    val keyCoverage =
      check(
        mappedKeys.map(_.field).toSet == lookup.keyFields.toSet,
        s"$prefix Lookup argument mappings must use every declared key field."
      )

    validated(unknown ::: repeated ::: missing ::: keyCoverage, mappings).map(_.toMap)
  }

  private def compileArgument[A](path: String, mapping: Lookup.Argument[A], expected: __Type)(
    leaf: (String, A, __Type) => Either[List[String], LookupArgument]
  ): Either[List[String], LookupArgument] = {
    val valueType = nullableType(expected)
    mapping match {
      case Lookup.Argument.Leaf(value)                                                   => leaf(path, value, valueType)
      case Lookup.Argument.ObjectMapping(_) if valueType.kind != __TypeKind.INPUT_OBJECT =>
        Left(List(s"$prefix Lookup argument '$path' maps an object into a non-input-object value."))
      case Lookup.Argument.ObjectMapping(fields)                                         =>
        compileObjectMapping(path, fields, valueType)(leaf)
    }
  }

  private def compileKey(path: String, key: Lookup.Key, valueType: __Type): Either[List[String], LookupArgument] =
    if (!lookup.keyFields.contains(key.field))
      Left(List(s"$prefix Lookup argument '$path' references undeclared key field '${key.field}'."))
    else if (keys.get(key.field).exists(field => !compatibleValueType(field._type, valueType)))
      Left(List(s"$prefix Lookup argument '$path' is incompatible with key field '${key.field}'."))
    else Right(LookupArgument.Key(key.field, valueType))

  private def compileBatch(path: String, batch: Lookup.Batch, valueType: __Type): Either[List[String], LookupArgument] =
    if (!valueType.isList) Left(List(s"$prefix Lookup argument '$path' maps a batch into a non-list value."))
    else
      compileArgument(path, batch.value, valueType.ofType.getOrElse(valueType))(compileKey).map(LookupArgument.Batch(_))

  private def compileObjectMapping[A](path: String, fields: List[(String, Lookup.Argument[A])], inputType: __Type)(
    leaf: (String, A, __Type) => Either[List[String], LookupArgument]
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
    val mappings    = collectErrors(fields.flatMap { case (name, value) =>
      inputFields
        .get(name)
        .toList
        .map(input => compileArgument(s"$path.$name", value, input._type)(leaf).map(name -> _))
    })
    validated(repeated ::: unknown ::: missing, mappings).map(LookupArgument.ObjectMapping(_))
  }

  private def correlationDiagnostics(field: __Field, correlation: Map[String, String]): List[String] =
    targetType.toList.flatMap { target =>
      val nullability = check(
        nullableType(field._type).ofType.exists(!_.isNullable),
        s"$prefix By-key lookup field 'Query.${lookup.field}' must return non-null items."
      )
      val coverage    = check(
        correlation.values.toList.sorted == lookup.keyFields.sorted,
        s"$prefix By-key lookup correlation must map every declared key field exactly once."
      )
      val values      = correlation.toList.flatMap { case (responseField, keyField) =>
        (fieldDefinition(target, responseField), keys.get(keyField)) match {
          case (None, _)                                                                      =>
            List(s"$prefix Lookup correlation field '${lookup.typeName}.$responseField' does not exist.")
          case _ if !lookup.keyFields.contains(keyField)                                      =>
            List(s"$prefix Lookup correlation references undeclared key field '$keyField'.")
          case (Some(response), Some(key)) if !compatibleValueType(response._type, key._type) =>
            List(
              s"$prefix Lookup correlation field '${lookup.typeName}.$responseField' is incompatible with key '$keyField'."
            )
          case _                                                                              => Nil
        }
      }
      nullability ::: coverage ::: values
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
}
