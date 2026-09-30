package caliban.gateway.internal.composition

import caliban.gateway._
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.introspection.adt._

private[composition] final class LookupCompilation private (subgraph: Source, lookup: Lookup) {
  private val prefix      = s"[${subgraph.name}]"
  private val schema      = subgraph.mapping.sourceRootType
  private val targetType  = schema.types.get(lookup.typeName)
  private val sourceField = fieldDefinition(schema.queryType, lookup.field)
  private val keys        = lookup.keyFields.flatMap(name => targetType.flatMap(fieldDefinition(_, name)).map(name -> _)).toMap

  def compile: Either[List[String], LookupOperation.GraphQLQuery] = {
    val targetDiagnostics = targetTypeDiagnostics ::: keyDiagnostics
    sourceField match {
      case None        => Left(targetDiagnostics :+ s"$prefix Lookup field 'Query.${lookup.field}' does not exist.")
      case Some(field) =>
        val resultType = nullableType(field._type)
        val compiled   = lookup match {
          case Lookup.Single(_, _, arguments) =>
            validated(
              shapeDiagnostics(isTarget(resultType), s"'${lookup.typeName}'"),
              compileFields(lookup.field, "argument", field.allArgs, arguments)(compileKey)
            ).map(LookupOperation.Single(lookup.field, _))
          case Lookup.ByKey(_, _, arguments)  =>
            val isTargetList =
              resultType.kind == __TypeKind.LIST && resultType.ofType.map(nullableType).exists(isTarget)
            validated(
              shapeDiagnostics(isTargetList, s"a list of '${lookup.typeName}'") ::: check(
                resultType.ofType.exists(!_.isNullable),
                s"$prefix By-key lookup field 'Query.${lookup.field}' must return non-null items."
              ),
              compileFields(lookup.field, "argument", field.allArgs, arguments)(compileBatch)
            ).map(LookupOperation.ByKey(lookup.field, _))
        }
        validated(targetDiagnostics, compiled)
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
    val missing =
      check(lookup.keyFields.nonEmpty, s"$prefix Lookup for '${lookup.typeName}' must declare at least one key field.")
    val invalid =
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
    missing ::: invalid
  }

  private def shapeDiagnostics(valid: Boolean, shape: String): List[String] =
    check(valid, s"$prefix Lookup field 'Query.${lookup.field}' must return $shape.")

  private def compileArgument[A, B](path: String, mapping: Lookup.Argument[A], expected: __Type)(
    leaf: (String, A, __Type) => Either[List[String], B]
  ): Either[List[String], Lookup.Argument[B]] = {
    val valueType = nullableType(expected)
    mapping match {
      case Lookup.Argument.Leaf(value)                                                   =>
        leaf(path, value, valueType).map(Lookup.Argument.Leaf(_))
      case Lookup.Argument.ObjectMapping(_) if valueType.kind != __TypeKind.INPUT_OBJECT =>
        Left(List(s"$prefix Lookup argument '$path' maps an object into a non-input-object value."))
      case Lookup.Argument.ObjectMapping(fields)                                         =>
        compileFields(path, "input field", valueType.allInputFields, fields)(leaf).map(Lookup.Argument.ObjectMapping(_))
    }
  }

  private def compileKey(path: String, key: Lookup.Key, valueType: __Type): Either[List[String], KeyArgument] =
    if (keys.get(key.field).exists(field => !compatibleValueType(field._type, valueType)))
      Left(List(s"$prefix Lookup argument '$path' is incompatible with key field '${key.field}'."))
    else Right(KeyArgument(subgraph.mapping.clientField(lookup.typeName, key.field), valueType))

  private def compileBatch(
    path: String,
    batch: Lookup.Batch,
    valueType: __Type
  ): Either[List[String], Lookup.Argument[KeyArgument]] =
    if (!valueType.isList) Left(List(s"$prefix Lookup argument '$path' maps a batch into a non-list value."))
    else compileArgument(path, batch.value, valueType.ofType.getOrElse(valueType))(compileKey)

  private def compileFields[A, B](
    path: String,
    noun: String,
    definitions: List[__InputValue],
    fields: List[(String, Lookup.Argument[A])]
  )(
    leaf: (String, A, __Type) => Either[List[String], B]
  ): Either[List[String], List[(String, Lookup.Argument[B])]] = {
    val byName   = definitions.map(definition => definition.name -> definition).toMap
    val mapped   = fields.map(_._1)
    val unknown  = mapped.collect {
      case name if !byName.contains(name) => s"$prefix Lookup $noun '$path.$name' does not exist."
    }
    val repeated = duplicates(mapped).map(name => s"$prefix Lookup argument '$path.$name' is mapped more than once.")
    val missing  = definitions.collect {
      case definition if !mapped.contains(definition.name) && isRequiredInput(definition) =>
        s"$prefix Required lookup $noun '$path.${definition.name}' has no mapping."
    }
    val mappings = collectErrors(fields.flatMap { case (name, value) =>
      byName.get(name).toList.map(input => compileArgument(s"$path.$name", value, input._type)(leaf).map(name -> _))
    })
    validated(unknown ::: repeated ::: missing, mappings)
  }
}

private[composition] object LookupCompilation {

  def compile(subgraph: Source, lookup: Lookup): Either[List[String], LookupOperation.GraphQLQuery] =
    new LookupCompilation(subgraph, lookup).compile

  def declarationDiagnostics(subgraph: Source): List[String] = {
    val federation = check(
      !subgraph.federation || subgraph.lookups.isEmpty,
      s"[${subgraph.name}] Ordinary GraphQL lookups cannot be declared on a Federation subgraph."
    )
    federation ::: duplicates(subgraph.lookups.map(_.typeName)).map(typeName =>
      s"[${subgraph.name}] More than one lookup is declared for type '$typeName'."
    )
  }
}
