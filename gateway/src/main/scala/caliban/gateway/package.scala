package caliban

import caliban.execution.Field
import caliban.ResponseValue.ObjectValue
import caliban.Value.StringValue
import caliban.introspection.adt.{ __InputValue, __Type, __TypeKind }
import caliban.parsing.Parser
import caliban.parsing.adt.{ Directive, OperationType, Selection }

package object gateway {
  private[gateway] final val TypenameField = "__typename"
  private[gateway] final val EntitiesField = "_entities"
  private[gateway] final val ServiceField  = "_service"
  private[gateway] final val ServiceType   = "_Service"
  private[gateway] final val AnyType       = "_Any"

  private[gateway] final val RepresentationsArgument = "representations"

  private[gateway] final val FederationIdentity     = "https://specs.apollo.dev/federation"
  private[gateway] final val LinkIdentity           = "https://specs.apollo.dev/link"
  private[gateway] final val AuthenticatedIdentity  = "https://specs.apollo.dev/authenticated"
  private[gateway] final val RequiresScopesIdentity = "https://specs.apollo.dev/requiresScopes"
  private[gateway] final val PolicyIdentity         = "https://specs.apollo.dev/policy"
  private[gateway] final val CostIdentity           = "https://specs.apollo.dev/cost"

  private[gateway] def isInclusionDirective(directive: Directive): Boolean =
    directive.name == "skip" || directive.name == "include"

  private[gateway] def isAbstractType(tpe: __Type): Boolean =
    tpe.kind == __TypeKind.INTERFACE || tpe.kind == __TypeKind.UNION

  private[gateway] def isCompositeType(tpe: __Type): Boolean =
    tpe.kind == __TypeKind.OBJECT || isAbstractType(tpe)

  private[gateway] def nullableType(tpe: __Type): __Type =
    if (tpe.kind == __TypeKind.NON_NULL) tpe.ofType.map(nullableType).getOrElse(tpe) else tpe

  private[gateway] def isRequiredInput(input: __InputValue): Boolean =
    !input._type.isNullable && input.defaultValue.isEmpty

  private[gateway] def parentTypeName(field: Field): String =
    field.parentType.flatMap(_.name).getOrElse("")

  private[gateway] def innerParentTypeName(field: Field): String =
    field.parentType.flatMap(_.innerType.name).getOrElse("")

  private[gateway] def responseNames(fields: Iterable[Field]): Set[String] =
    fields.iterator.map(_.aliasedName).toSet

  private[gateway] def errorCode(code: String): Option[ObjectValue] =
    Some(ObjectValue(List("code" -> StringValue(code))))

  private[gateway] def traverseOption[A, B](values: Iterable[A])(f: A => Option[B]): Option[List[B]] = {
    val collected = List.newBuilder[B]
    val iterator  = values.iterator
    while (iterator.hasNext)
      f(iterator.next()) match {
        case Some(value) => collected += value
        case None        => return None
      }
    Some(collected.result())
  }

  private[gateway] def traverseEither[E, A, B](values: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    values
      .foldLeft[Either[E, List[B]]](Right(Nil))((result, value) =>
        result.flatMap(collected => f(value).map(_ :: collected))
      )
      .map(_.reverse)

  private[gateway] def check(valid: Boolean, message: String): List[String] =
    if (valid) Nil else message :: Nil

  private[gateway] def validateAll[A](results: List[Either[String, A]]): Either[List[String], List[A]] = {
    val errors = results.collect { case Left(error) => error }
    if (errors.nonEmpty) Left(errors) else Right(results.collect { case Right(value) => value })
  }

  private[gateway] def stringValues(values: List[InputValue]): Option[List[String]] =
    traverseOption(values) {
      case StringValue(value) => Some(value)
      case _                  => None
    }

  private[gateway] def duplicates(names: List[String]): List[String] =
    names.groupBy(identity).collect { case (name, occurrences) if occurrences.size > 1 => name }.toList

  private[gateway] def formatSources(sources: Iterable[String]): String =
    sources.toList.distinct.sorted.map(source => s"'$source'").mkString(", ")

  private[gateway] def fieldDiagnosticPrefix(
    operation: Option[OperationType],
    typeName: String,
    fieldName: String
  ): String =
    operation.fold(s"[type $typeName.$fieldName]")(value => s"[${value.toString.toLowerCase}.$fieldName]")

  private[gateway] def parseFieldSet(value: String): Option[List[Selection]] =
    parseSelectionSet(s"{ $value }")

  private[gateway] def parseSelectionSet(query: String): Option[List[Selection]] =
    Parser.parseQuery(query).toOption.flatMap(_.operationDefinition(None)).map(_.selectionSet)
}
