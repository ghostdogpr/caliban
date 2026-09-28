package caliban.gateway.internal.composition

import caliban.InputValue
import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue }
import caliban.Value._
import caliban.gateway._
import caliban.gateway.internal.composition.SchemaComposer.PreparedSubgraph
import caliban.introspection.adt._
import caliban.parsing.adt.{ Directive, Document }
import caliban.rendering.DocumentRenderer
import caliban.schema.RootType
import caliban.validation.{ Context, Validator }

import scala.collection.compat._
import scala.collection.immutable.ListMap

/**
 * Selects directive definitions and applications retained in the composed schema.
 */
private[gateway] object DirectiveComposition {

  def compile(subgraphs: List[PreparedSubgraph]): ComposedDirectives = {
    val sourceDirectives             = subgraphs.map(SourceDirectives(_))
    val declarations                 = sourceDirectives.flatMap(_.composeDeclarations)
    val selected                     = sourceDirectives.flatMap(info => info.definitions.filter(value => info.selected(value.key)))
    val renamed                      = groupNonEmpty(selected)(_.key).toList.flatMap { case (key, values) =>
      val name =
        if (values.exists(_.definition.name == key.member)) key.member
        else values.minBy(_.definition.name).definition.name
      values.map(value => value.copy(definition = value.definition.copy(name = name)))
    }
    val composedNames                = renamed.map(value => value.key -> value.definition.name).toMap
    val nameCollisions               = composedNames.toList
      .groupMap(_._2)(_._1)
      .collect {
        case (name, keys) if keys.size > 1 =>
          val identities = keys.map(_.identity.fold("an unlinked definition")(id => s"'$id'")).toList.distinct.sorted
          s"[directive @$name] Linked directive identities collide: ${identities.mkString(" and ")}."
      }
      .toList
    val hidden                       =
      sourceDirectives.flatMap(info => info.subgraph.hidden.flatMap(info.composedCoordinate(composedNames))).toSet
    val selectedDefinitions          = renamed.map { value =>
      val name = value.definition.name
      value.copy(definition =
        value.definition.copy(args =
          value.definition.args(_).filterNot(argument => hidden(DirectiveArgumentCoordinate(name, argument.name)))
        )
      )
    }
    val definitionDiagnostics        = groupNonEmpty(selectedDefinitions)(_.key).values.collect {
      case values if values.map(value => definitionSignature(value.definition)).distinct.size > 1 =>
        s"[directive @${values.head.definition.name}] Definitions are incompatible between subgraphs: ${formatSources(values.map(_.source))}."
    }.toList
    val rawApplications              = sourceDirectives.flatMap(_.applications(composedNames))
    val (invalid, validApplications) = rawApplications.map(compileApplication(_, hidden)).partitionMap(identity)
    val applicationErrors            = invalid.flatten
    val applicationGroups            = groupNonEmpty(validApplications)(value => value.coordinate -> value.local.key).values.toList

    val diagnostics = sourceDirectives.flatMap(_.diagnostics) :::
      declarations.collect { case Left(error) => error } ::: nameCollisions ::: definitionDiagnostics :::
      applicationErrors

    new ComposedDirectives(
      selectedDefinitions.sortBy(value => value.definition.name -> value.source),
      applicationGroups,
      hidden,
      diagnostics
    )
  }

  def schemaDirectives(document: Document): List[Directive] =
    document.schemaDefinition.toList.flatMap(_.directives)

  def linkedFeatures(document: Document): List[LinkedFeature] =
    schemaDirectives(document).filter(_.name == "link").flatMap { directive =>
      stringArgument(directive.arguments, "url").flatMap { url =>
        url.takeWhile(character => character != '?' && character != '#').stripSuffix("/") match {
          case LinkUrlPattern(identity, name, major, minor) =>
            val imports   = directive.arguments.get("import").toList.flatMap(coercedList).flatMap(importedName)
            val namespace = stringArgument(directive.arguments, "as").fold(name)(_.stripPrefix("@"))
            Some(LinkedFeature(identity, name, FeatureVersion(major.toInt, minor.toInt), namespace, imports))
          case _                                            => None
        }
      }
    }

  final case class FeatureVersion(major: Int, minor: Int) {
    def atLeast(requiredMajor: Int, requiredMinor: Int): Boolean =
      major > requiredMajor || major == requiredMajor && minor >= requiredMinor
  }

  final case class LinkedFeature private[DirectiveComposition] (
    identity: String,
    name: String,
    version: FeatureVersion,
    namespace: String,
    imports: List[ImportedName]
  ) {
    def directiveNames(name: String): Set[String] = {
      val imported  = imports.collect { case value if value.isDirective && value.name == name => value.alias }.toSet
      val qualified = if (this.name == name) Set(namespace) else Set(s"${namespace}__$name")
      imported ++ qualified
    }

    /**
     * Resolves a local alias or namespaced name to the directive's original name in the linked feature.
     */
    def sourceDirective(localName: String): Option[String] =
      imports.collectFirst { case value if value.isDirective && value.alias == localName => value.name }.orElse {
        val prefix = namespace + "__"
        if (localName == namespace) Some(name)
        else if (localName.startsWith(prefix) && localName.length > prefix.length) Some(localName.stripPrefix(prefix))
        else None
      }
  }

  /**
   * A schema element where a directive is applied, such as Product.price or Query.product(id:).
   * The location distinguishes elements with the same display name, such as output and input fields.
   */
  sealed trait Coordinate {
    def display: String
    def location: __DirectiveLocation
  }

  case object SchemaCoordinate extends Coordinate {
    val display  = "schema"
    val location = __DirectiveLocation.SCHEMA
  }

  final case class TypeCoordinate(typeName: String, location: __DirectiveLocation) extends Coordinate {
    val display = typeName
  }

  final case class FieldCoordinate(typeName: String, fieldName: String) extends Coordinate {
    def display  = s"$typeName.$fieldName"
    val location = __DirectiveLocation.FIELD_DEFINITION
  }

  final case class ArgumentCoordinate(typeName: String, fieldName: String, argumentName: String) extends Coordinate {
    def display  = s"$typeName.$fieldName($argumentName:)"
    val location = __DirectiveLocation.ARGUMENT_DEFINITION
  }

  final case class InputFieldCoordinate(typeName: String, fieldName: String) extends Coordinate {
    def display  = s"$typeName.$fieldName"
    val location = __DirectiveLocation.INPUT_FIELD_DEFINITION
  }

  final case class EnumValueCoordinate(typeName: String, valueName: String) extends Coordinate {
    def display  = s"$typeName.$valueName"
    val location = __DirectiveLocation.ENUM_VALUE
  }

  final case class DirectiveArgumentCoordinate(directiveName: String, argumentName: String) extends Coordinate {
    def display  = s"@$directiveName($argumentName:)"
    val location = __DirectiveLocation.ARGUMENT_DEFINITION
  }

  final class ComposedDirectives private[DirectiveComposition] (
    private val selectedDefinitions: List[LocalDefinition],
    private val applicationGroups: List[::[Application]],
    val hidden: Set[Coordinate],
    val diagnostics: List[String]
  ) {
    private val applications = applicationGroups
      .sortBy(_.head.directive.name)
      .groupMap(_.head.coordinate)(mergeApplications)
      .map { case (coordinate, values) => coordinate -> values.flatten }

    // ID may be referenced only by a retained directive argument and otherwise absent from the schema.
    def additionalTypes: List[__Type] =
      selectedDefinitions.iterator
        .flatMap(_.definition.allArgs.iterator.map(_._type.innerType))
        .find(tpe => tpe.kind == __TypeKind.SCALAR && tpe.name.contains("ID"))
        .toList

    def definitions(rewrite: __Type => __Type): List[__Directive] =
      selectedDefinitions.distinctBy(_.key).map { selected =>
        val definition = selected.definition
        definition.copy(args =
          includeDeprecated =>
            definition
              .args(includeDeprecated)
              .map(argument =>
                argument.copy(
                  `type` = () => rewrite(argument._type),
                  directives = attach(argument.directives, DirectiveArgumentCoordinate(definition.name, argument.name))
                )
              )
        )
      }

    def attachType(tpe: __Type, name: String): __Type =
      tpe.copy(directives = attach(tpe.directives, TypeCoordinate(name, typeLocation(tpe.kind))))

    def attachField(parent: String, field: __Field): __Field =
      field.copy(
        directives = attach(field.directives, FieldCoordinate(parent, field.name)),
        args = includeDeprecated =>
          field
            .args(includeDeprecated)
            .map(argument =>
              argument.copy(directives =
                attach(
                  argument.directives,
                  ArgumentCoordinate(parent, field.name, argument.name)
                )
              )
            )
      )

    def attachInputField(parent: String, field: __InputValue): __InputValue =
      field.copy(directives = attach(field.directives, InputFieldCoordinate(parent, field.name)))

    def attachEnumValue(parent: String, value: __EnumValue): __EnumValue =
      value.copy(directives = attach(value.directives, EnumValueCoordinate(parent, value.name)))

    // Conflicts on elements removed from the composed schema are irrelevant.
    def schemaDiagnostics(rootType: RootType): List[String] = {
      val visible = applicationGroups.filter(values => coordinateExists(rootType, values.head.coordinate))
      visible.flatMap(applicationConflicts) ::: visibilityDiagnostics(rootType, visible.flatten)
    }

    private def attach(existing: Option[List[Directive]], coordinate: Coordinate): Option[List[Directive]] = {
      val values = builtIn(existing) ::: applications.getOrElse(coordinate, Nil)
      if (values.isEmpty) None else Some(values)
    }

    private def visibilityDiagnostics(rootType: RootType, applications: List[Application]): List[String] = {
      def missing(source: String, directive: String, context: String, references: Set[Coordinate]): List[String] =
        references.toList.collect {
          case reference if !coordinateExists(rootType, reference) =>
            val kind = reference match {
              case _: InputFieldCoordinate => "input field"
              case _: EnumValueCoordinate  => "enum value"
              case _                       => "input type"
            }
            s"[$source] Composed directive '@$directive' at '$context' references non-visible $kind '${reference.display}'."
        }

      val definitionDiagnostics  = selectedDefinitions.flatMap { selected =>
        selected.definition.allArgs.flatMap { argument =>
          missing(
            selected.source,
            selected.definition.name,
            s"@${selected.definition.name}(${argument.name}:)",
            SchemaMapping.namedReference(argument._type.innerType) ++
              argument.parsedDefaultValue.toList.flatMap(SchemaMapping.inputReferences(argument._type, _))
          )
        }
      }
      val applicationDiagnostics = applications.flatMap(application =>
        application.local.definition.allArgs.flatMap { argument =>
          application.directive.arguments
            .get(argument.name)
            .toList
            .flatMap(value =>
              missing(
                application.local.source,
                application.directive.name,
                application.coordinate.display,
                SchemaMapping.inputReferences(argument._type, value)
              )
            )
        }
      )

      definitionDiagnostics ::: applicationDiagnostics
    }
  }

  final case class ImportedName(name: String, alias: String, isDirective: Boolean)
  private final case class DirectiveKey(identity: Option[String], member: String)
  private final case class LocalDefinition(source: String, key: DirectiveKey, definition: __Directive)
  private final case class Application(local: LocalDefinition, coordinate: Coordinate, directive: Directive)
  private final case class SourceDirectives(subgraph: PreparedSubgraph) {
    private val names                         = subgraph.directiveNames
    private val features                      = names.features
    private val federationTransportDirectives =
      if (features.exists(_.identity == FederationIdentity)) FederationTransportDirectiveNames else Set.empty[String]
    private val linkedKeys                    = subgraph.rootType.additionalDirectives.map { definition =>
      definition -> features.flatMap { feature =>
        feature.sourceDirective(definition.name).map(DirectiveKey(Some(feature.identity), _))
      }.distinct
    }
    val definitions: List[LocalDefinition]    = linkedKeys.map { case (definition, keys) =>
      LocalDefinition(subgraph.name, keys.headOption.getOrElse(DirectiveKey(None, definition.name)), definition)
    }
    private val definitionsByName             = definitions.map(local => local.definition.name -> local).toMap

    val diagnostics: List[String] = linkedKeys.collect { case (definition, keys @ _ :: _ :: _) =>
      val identities = keys.flatMap(_.identity).sorted.map(value => s"'$value'").mkString(" and ")
      s"[${subgraph.name}] Directive '@${definition.name}' resolves to multiple linked feature identities: $identities."
    }

    val composeDeclarations: List[Either[String, DirectiveKey]] =
      subgraph.applications(FederationCompilation.FederationDirective.ComposeDirective).map { application =>
        stringArgument(application.directive.arguments, "name") match {
          case Some(value) if value.startsWith("@") && value.length > 1 => composedDefinition(value.drop(1))
          case _                                                        =>
            Left(s"[${subgraph.name}] The composeDirective 'name' argument must start with '@' and name a directive.")
        }
      }

    private def composedDefinition(localName: String): Either[String, DirectiveKey] =
      definitionsByName.get(localName) match {
        case None                                                     =>
          Left(s"[${subgraph.name}] Composed directive '@$localName' is not defined by this subgraph.")
        case Some(definition) if definition.key.identity.isEmpty      =>
          Left(s"[${subgraph.name}] Composed directive '@$localName' must be imported from a linked custom feature.")
        case Some(definition) if isTransportDirective(definition.key) =>
          Left(s"[${subgraph.name}] Federation transport directive '@$localName' cannot be composed.")
        case Some(definition)                                         => Right(definition.key)
      }

    val selected: Set[DirectiveKey] =
      composeDeclarations.collect { case Right(value) => value }.toSet ++
        (if (subgraph.federation) Set(DirectiveKey(Some(FederationIdentity), "tag"))
         else
           definitions.iterator.filterNot { value =>
             val name = value.definition.name
             BuiltInDirectiveNames(name) || names.hidden(name) || federationTransportDirectives(name) ||
             value.key.identity.exists(id => ReservedFeatureIdentities(id) || id == FederationIdentity)
           }
             .map(_.key)
             .toSet)

    private def isTransportDirective(key: DirectiveKey): Boolean =
      key.identity.exists(id => id == FederationIdentity && key.member != "tag" || ReservedFeatureIdentities(id))

    private def selectedDefinition(composedNames: Map[DirectiveKey, String], name: String) =
      definitionsByName.get(name).filter(local => selected(local.key)).flatMap { local =>
        composedNames.get(local.key).map(local -> _)
      }

    def composedCoordinate(composedNames: Map[DirectiveKey, String])(coordinate: Coordinate): Option[Coordinate] =
      coordinate match {
        case TypeCoordinate(name, __DirectiveLocation.OBJECT) if subgraph.isInterfaceObject(name) =>
          Some(TypeCoordinate(name, __DirectiveLocation.INTERFACE))
        case DirectiveArgumentCoordinate(directive, argument)                                     =>
          selectedDefinition(composedNames, directive).map { case (_, name) =>
            DirectiveArgumentCoordinate(name, argument)
          }
        case other                                                                                => Some(other)
      }

    def applications(composedNames: Map[DirectiveKey, String]): List[Application] =
      subgraph.directiveApplications.flatMap { application =>
        composedCoordinate(composedNames)(application.coordinate).toList.flatMap { coordinate =>
          application.directives.flatMap { directive =>
            selectedDefinition(composedNames, directive.name).map { case (local, name) =>
              Application(local, coordinate, directive.copy(name = name))
            }
          }
        }
      }
  }

  private def mergeApplications(values: ::[Application]): List[Directive] =
    (if (values.head.local.definition.isRepeatable) values.toList.distinctBy(_.directive.arguments) else values.take(1))
      .map(_.directive)

  private def applicationConflicts(values: ::[Application]): List[String] = {
    val first = values.head
    check(
      first.local.definition.isRepeatable || values.map(_.directive.arguments).distinct.size <= 1,
      s"[${first.coordinate.display}] Non-repeatable directive '@${first.directive.name}' has incompatible applications between subgraphs: ${formatSources(values.map(_.local.source))}."
    )
  }

  private def compileApplication(application: Application, hidden: Set[Coordinate]) = {
    val Application(LocalDefinition(source, _, definition), coordinate, directive) = application

    val location = check(
      definition.locations(coordinate.location),
      s"[$source] Composed directive '@${directive.name}' does not support ${coordinate.location} at '${coordinate.display}'."
    )
    val label    = s"composed directive '@${directive.name}'"
    validated(location, validateArguments(source, label, coordinate, directive, definition)).map { valid =>
      val visible = valid.arguments.filterNot { case (name, _) =>
        hidden(DirectiveArgumentCoordinate(directive.name, name))
      }
      application.copy(directive = valid.copy(arguments = visible))
    }
  }

  def validateArguments(
    source: String,
    label: String,
    coordinate: Coordinate,
    directive: Directive,
    definition: __Directive
  ): Either[List[String], Directive] = {
    val arguments   = definition.allArgs.map(argument => argument.name -> argument).toMap
    val unknown     = directive.arguments.keySet
      .diff(arguments.keySet)
      .toList
      .sorted
      .map(name => s"[$source] Unknown argument '$name' on $label at '${coordinate.display}'.")
    val missing     = definition.allArgs.collect {
      case argument if isRequiredInput(argument) && !directive.arguments.contains(argument.name) =>
        s"[$source] Required argument '${argument.name}' is missing on $label at '${coordinate.display}'."
    }
    val valueErrors = directive.arguments.toList.flatMap { case (name, value) =>
      arguments
        .get(name)
        .toList
        .flatMap(argument =>
          Validator
            .validateInputValues(argument, value, Context.empty, s"Argument '$name' of directive '@${directive.name}'")
            .left
            .toOption
            .map(error => s"[$source] ${error.getMessage}")
        )
    }
    val errors      = unknown ::: missing ::: valueErrors

    if (errors.nonEmpty) Left(errors)
    else {
      val normalizedArgs = definition.allArgs.flatMap { argument =>
        directive.arguments
          .get(argument.name)
          .orElse(argument.parsedDefaultValue)
          .map(value => argument.name -> canonicalValue(argument._type, value))
      }
      Right(directive.copy(arguments = ListMap(normalizedArgs: _*)))
    }
  }

  private def canonicalValue(tpe: __Type, value: InputValue): InputValue =
    tpe.kind match {
      case __TypeKind.NON_NULL                             => tpe.ofType.fold(value)(canonicalValue(_, value))
      case __TypeKind.LIST                                 =>
        value match {
          case NullValue              => NullValue
          case InputListValue(values) =>
            InputListValue(tpe.ofType.fold(values)(nested => values.map(canonicalValue(nested, _))))
          case singleton              =>
            InputListValue(tpe.ofType.fold(singleton :: Nil)(nested => canonicalValue(nested, singleton) :: Nil))
        }
      case __TypeKind.INPUT_OBJECT                         =>
        value match {
          case InputObjectValue(fields) =>
            val normalized = tpe.allInputFields.flatMap { field =>
              fields
                .get(field.name)
                .orElse(field.parsedDefaultValue)
                .map(value => field.name -> canonicalValue(field._type, value))
            }
            InputObjectValue(ListMap(normalized: _*))
          case other                    => other
        }
      case __TypeKind.SCALAR if tpe.name.contains("Float") =>
        value match {
          case number: IntValue   => FloatValue(canonicalDecimal(BigDecimal(number.toBigInt)))
          case number: FloatValue => FloatValue(canonicalDecimal(number.toBigDecimal))
          case other              => other
        }
      case _                                               => value
    }

  private def canonicalDecimal(value: BigDecimal): BigDecimal =
    BigDecimal(value.bigDecimal.stripTrailingZeros)

  private def coordinateExists(rootType: RootType, coordinate: Coordinate): Boolean =
    coordinate match {
      case SchemaCoordinate                                         => true
      case TypeCoordinate(typeName, _)                              => rootType.types.contains(typeName)
      case FieldCoordinate(typeName, fieldName)                     =>
        rootType.types.get(typeName).flatMap(fieldDefinition(_, fieldName)).nonEmpty
      case ArgumentCoordinate(typeName, fieldName, argumentName)    =>
        rootType.types
          .get(typeName)
          .flatMap(fieldDefinition(_, fieldName))
          .exists(_.allArgs.exists(_.name == argumentName))
      case InputFieldCoordinate(typeName, fieldName)                =>
        rootType.types.get(typeName).flatMap(inputFieldDefinition(_, fieldName)).nonEmpty
      case EnumValueCoordinate(typeName, valueName)                 =>
        rootType.types.get(typeName).exists(_.allEnumValues.exists(_.name == valueName))
      case DirectiveArgumentCoordinate(directiveName, argumentName) =>
        rootType.additionalDirectives
          .find(_.name == directiveName)
          .exists(_.allArgs.exists(_.name == argumentName))
    }

  def typeLocation(kind: __TypeKind): __DirectiveLocation =
    kind match {
      case __TypeKind.SCALAR       => __DirectiveLocation.SCALAR
      case __TypeKind.OBJECT       => __DirectiveLocation.OBJECT
      case __TypeKind.INTERFACE    => __DirectiveLocation.INTERFACE
      case __TypeKind.UNION        => __DirectiveLocation.UNION
      case __TypeKind.ENUM         => __DirectiveLocation.ENUM
      case __TypeKind.INPUT_OBJECT => __DirectiveLocation.INPUT_OBJECT
      case _                       => __DirectiveLocation.OBJECT
    }

  final case class InputSignature(typeName: String, defaultValue: Option[InputValue])

  def inputSignature(value: __InputValue): InputSignature =
    InputSignature(
      DocumentRenderer.renderTypeName(value._type),
      value.parsedDefaultValue.map(canonicalValue(value._type, _))
    )

  private final case class DefinitionSignature(
    repeatable: Boolean,
    locations: Set[__DirectiveLocation],
    arguments: Map[String, (InputSignature, Boolean, Option[String])]
  )

  private def definitionSignature(definition: __Directive): DefinitionSignature = {
    val arguments = definition.allArgs.map { argument =>
      argument.name -> ((inputSignature(argument), argument.isDeprecated, argument.deprecationReason))
    }
    DefinitionSignature(definition.isRepeatable, definition.locations, arguments.toMap)
  }

  private def importedName(value: InputValue): Option[ImportedName] =
    value match {
      case StringValue(name)        =>
        Some(ImportedName(name.stripPrefix("@"), name.stripPrefix("@"), name.startsWith("@")))
      case InputObjectValue(fields) =>
        stringArgument(fields, "name").map { name =>
          val alias = stringArgument(fields, "as").getOrElse(name)
          ImportedName(name.stripPrefix("@"), alias.stripPrefix("@"), name.startsWith("@"))
        }
      case _                        => None
    }

  private val LinkUrlPattern                                        = "(?s)(.*/([^/]+))/v([0-9]{1,9})\\.([0-9]{1,9})".r
  // Other source applications reach the composed schema only through validated composed applications.
  def builtIn(directives: Option[List[Directive]]): List[Directive] =
    directives.getOrElse(Nil).filter(directive => BuiltInDirectiveNames(directive.name))

  private val BuiltInDirectiveNames             = Set("skip", "include", "deprecated", "specifiedBy", "oneOf")
  private val FederationTransportDirectiveNames =
    FederationCompilation.FederationDirective.all.map(_.name).toSet + "link"
  private val ReservedFeatureIdentities         = FederationCompilation.FederationDirective.linkedSpecIdentities + LinkIdentity
}
