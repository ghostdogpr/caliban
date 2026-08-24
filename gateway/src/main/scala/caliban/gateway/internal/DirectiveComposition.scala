package caliban.gateway.internal

import caliban.InputValue
import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue }
import caliban.Value._
import caliban.introspection.adt._
import caliban.parsing.Parser
import caliban.parsing.adt.Definition.TypeSystemExtension.SchemaExtension
import caliban.parsing.adt.{ Directive, Document }
import caliban.rendering.DocumentRenderer
import caliban.schema.RootType
import caliban.validation.{ Context, Validator }

import scala.collection.immutable.ListMap

private[gateway] object DirectiveComposition {

  val FederationIdentity = "https://specs.apollo.dev/federation"

  final case class FeatureVersion(major: Int, minor: Int) {
    def atLeast(requiredMajor: Int, requiredMinor: Int): Boolean =
      major > requiredMajor || major == requiredMajor && minor >= requiredMinor
  }

  final case class LinkedFeature private[DirectiveComposition] (
    directive: Directive,
    identity: String,
    name: String,
    version: FeatureVersion,
    namespace: String,
    imports: List[ImportedName]
  ) {
    def directiveNames(name: String): Set[String] = {
      val imported  = imports.collect { case value if value.directive && value.name == name => value.alias }.toSet
      val qualified = if (this.name == name) Set(namespace) else Set(s"${namespace}__$name")
      imported ++ qualified
    }

    def sourceDirective(localName: String): Option[String] =
      imports.collectFirst { case value if value.directive && value.alias == localName => value.name }.orElse {
        val prefix = namespace + "__"
        if (localName == namespace) Some(name)
        else if (localName.startsWith(prefix) && localName.length > prefix.length) Some(localName.stripPrefix(prefix))
        else None
      }
  }

  final case class Source(schema: SchemaContribution, protocolDirectives: Set[String])

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
    val display  = s"$typeName.$fieldName"
    val location = __DirectiveLocation.FIELD_DEFINITION
  }

  final case class ArgumentCoordinate(typeName: String, fieldName: String, argumentName: String) extends Coordinate {
    val display  = s"$typeName.$fieldName($argumentName:)"
    val location = __DirectiveLocation.ARGUMENT_DEFINITION
  }

  final case class InputFieldCoordinate(typeName: String, fieldName: String) extends Coordinate {
    val display  = s"$typeName.$fieldName"
    val location = __DirectiveLocation.INPUT_FIELD_DEFINITION
  }

  final case class EnumValueCoordinate(typeName: String, valueName: String) extends Coordinate {
    val display  = s"$typeName.$valueName"
    val location = __DirectiveLocation.ENUM_VALUE
  }

  final case class DirectiveArgumentCoordinate(directiveName: String, argumentName: String) extends Coordinate {
    val display  = s"@$directiveName($argumentName:)"
    val location = __DirectiveLocation.ARGUMENT_DEFINITION
  }

  final class Result private[DirectiveComposition] (
    private val hiddenBySource: Map[String, Set[String]],
    private val selectedDefinitions: List[SelectedDefinition],
    private val applications: Map[Coordinate, List[Directive]],
    private val validApplications: List[CompiledApplication],
    private val deferredDiagnostics: List[(Coordinate, String)],
    val diagnostics: List[String]
  ) {
    def hidden(source: String): Set[String] =
      hiddenBySource.getOrElse(source, Set.empty)

    def schemaDirectives: List[Directive] =
      applications.getOrElse(SchemaCoordinate, Nil)

    def additionalTypes: List[__Type] =
      selectedDefinitions
        .flatMap(_.definition.allArgs.map(_._type.innerType))
        .filter(tpe => tpe.kind == __TypeKind.SCALAR && tpe.name.contains("ID"))
        .take(1)

    def definitions(rewrite: __Type => __Type): List[__Directive] =
      selectedDefinitions
        .groupBy(_.key)
        .valuesIterator
        .flatMap(_.sortBy(_.source).headOption)
        .toList
        .sortBy(_.definition.name)
        .map { selected =>
          val definition = selected.definition
          definition.copy(args =
            includeDeprecated =>
              definition
                .args(includeDeprecated)
                .map(argument =>
                  argument.copy(
                    `type` = () => rewrite(argument._type),
                    directives = attach(
                      argument.directives,
                      DirectiveArgumentCoordinate(definition.name, argument.name)
                    )
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

    def finalDiagnostics(rootType: RootType): List[String] = {
      val conflicts = deferredDiagnostics.collect {
        case (coordinate, diagnostic) if coordinateExists(rootType, coordinate) => diagnostic
      }
      (conflicts ::: visibilityDiagnostics(rootType)).distinct.sorted
    }

    private def attach(existing: Option[List[Directive]], coordinate: Coordinate): Option[List[Directive]] = {
      val values = existing.getOrElse(Nil) ::: applications.getOrElse(coordinate, Nil)
      if (values.isEmpty) None else Some(values)
    }

    private def visibilityDiagnostics(rootType: RootType): List[String] = {
      def namedType(tpe: __Type): __Type                                                            = tpe.ofType.fold(tpe)(namedType)
      def hiddenType(source: String, directive: String, context: String, tpe: __Type): List[String] = {
        val named = namedType(tpe)
        named.name.toList.collect {
          case name if !rootType.types.contains(name) =>
            s"[$source] Composed directive '@$directive' at '$context' references non-visible input type '$name'."
        }
      }
      def references(
        source: String,
        directive: String,
        context: String,
        expected: __Type,
        value: InputValue
      ): List[String] = {
        val found  = SchemaCoordinateMapping.inputCoordinateReferences(expected, value)
        val types  = found.inputTypes.toList.sorted.collect {
          case name if !rootType.types.contains(name) =>
            s"[$source] Composed directive '@$directive' at '$context' references non-visible input type '$name'."
        }
        val fields = found.inputFields.toList.sorted.collect {
          case (typeName, fieldName)
              if !rootType.types.get(typeName).exists(_.allInputFields.exists(_.name == fieldName)) =>
            s"[$source] Composed directive '@$directive' at '$context' references non-visible input field '$typeName.$fieldName'."
        }
        val enums  = found.enumValues.toList.sorted.collect {
          case (typeName, valueName)
              if !rootType.types.get(typeName).exists(_.allEnumValues.exists(_.name == valueName)) =>
            s"[$source] Composed directive '@$directive' at '$context' references non-visible enum value '$typeName.$valueName'."
        }
        types ::: fields ::: enums
      }

      val definitionDiagnostics  = selectedDefinitions.flatMap { selected =>
        selected.definition.allArgs.flatMap { argument =>
          val context = s"@${selected.definition.name}(${argument.name}:)"
          hiddenType(selected.source, selected.definition.name, context, argument._type) :::
            argument.defaultValue.toList.flatMap(value =>
              Parser
                .parseInputValue(value)
                .toOption
                .toList
                .flatMap(
                  references(selected.source, selected.definition.name, context, argument._type, _)
                )
            )
        }
      }
      val applicationDiagnostics = validApplications
        .filter(application => coordinateExists(rootType, application.coordinate))
        .flatMap(application =>
          application.definition.allArgs.flatMap { argument =>
            application.directive.arguments
              .get(argument.name)
              .toList
              .flatMap(value =>
                references(
                  application.source,
                  application.directive.name,
                  application.coordinate.display,
                  argument._type,
                  value
                )
              )
          }
        )

      definitionDiagnostics ::: applicationDiagnostics
    }
  }

  def compile(sources: List[Source]): Result = {
    val ordered               = sources.sortBy(_.schema.name)
    val sourceInfo            = ordered.map(source => source.schema.name -> SourceInfo(source)).toMap
    val definitions           = sourceInfo.valuesIterator.toList.flatMap(_.definitions)
    val declarations          = sourceInfo.valuesIterator.toList.flatMap(_.composeDeclarations)
    val selectedKeys          = sourceInfo.valuesIterator.flatMap(_.ordinarySelections).toSet ++
      definitions.iterator.collect {
        case definition if definition.key == DirectiveKey(FederationIdentity, "tag") =>
          definition.key
      }.toSet ++ declarations.collect { case Right(value) => value }
    val selected              = definitions.filter(definition => selectedKeys(definition.key))
    val composedNames         = selected
      .groupBy(_.key)
      .map { case (key, values) =>
        val names = values.map(_.localName).distinct.sorted
        key -> (if (names.contains(key.member)) key.member else names.headOption.getOrElse(key.member))
      }
    val nameCollisions        = composedNames.toList
      .groupBy(_._2)
      .collect {
        case (name, values) if values.map(_._1).distinct.size > 1 =>
          val identities = values.map(_._1.identity).distinct.sorted.map(value => s"'$value'").mkString(" and ")
          s"[directive @$name] Linked directive identities collide: $identities."
      }
      .toList
    val selectedDefinitions   = selected.map(value =>
      SelectedDefinition(
        value.source,
        value.key,
        value.definition.copy(name = composedNames.getOrElse(value.key, value.localName))
      )
    )
    val definitionDiagnostics = selectedDefinitions
      .groupBy(_.key)
      .collect {
        case (_, values) if values.map(value => definitionSignature(value.definition)).distinct.size > 1 =>
          val name    = values.headOption.map(_.definition.name).getOrElse("")
          val sources = SchemaComposition.formatSources(values.map(_.source))
          s"[directive @$name] Definitions are incompatible between subgraphs: $sources."
      }
      .toList
    val rawApplications       = sourceInfo.valuesIterator.toList.flatMap(_.applications(selectedKeys, composedNames))
    val compiled              = rawApplications.map(compileApplication)
    val applicationErrors     = compiled.collect { case Left(errors) => errors }.flatten
    val validApplications     = compiled.collect { case Right(value) => value }
    val definitionByKey       = selectedDefinitions.groupBy(_.key).flatMap { case (key, values) =>
      values.sortBy(_.source).headOption.map(key -> _.definition)
    }
    val applicationsByKey     = validApplications
      .groupBy(application => application.coordinate -> application.key)
      .toList
    val mergedApplications    = applicationsByKey.map { case ((coordinate, key), values) =>
      val definition = definitionByKey.get(key)
      val sorted     = values.sortBy(value => (value.source, value.ordinal))
      val selected   =
        if (definition.exists(_.isRepeatable)) {
          val maximumBySignature  = sorted
            .groupBy(value => value.source -> applicationSignature(value.directive))
            .toList
            .groupBy(_._1._2)
            .map { case (signature, occurrences) => signature -> occurrences.map(_._2.size).max }
          var retainedBySignature = Map.empty[List[(String, InputValue)], Int]
          sorted.filter { value =>
            val signature = applicationSignature(value.directive)
            val retained  = retainedBySignature.getOrElse(signature, 0)
            if (retained < maximumBySignature(signature)) {
              retainedBySignature = retainedBySignature.updated(signature, retained + 1)
              true
            } else false
          }
        } else sorted.headOption.toList
      coordinate -> selected.map(_.directive)
    }
      .groupBy(_._1)
      .map { case (coordinate, values) => coordinate -> values.flatMap(_._2) }
    val deferredDiagnostics   = applicationsByKey.flatMap { case ((coordinate, key), values) =>
      definitionByKey.get(key).toList.flatMap { definition =>
        if (definition.isRepeatable) Nil
        else {
          val bySourceDuplicates = values
            .groupBy(_.source)
            .collect {
              case (source, occurrences) if occurrences.size > 1 =>
                coordinate ->
                  s"[$source] Non-repeatable directive '@${definition.name}' is applied more than once at '${coordinate.display}'."
            }
            .toList
          val signatures         = values.map(value => applicationSignature(value.directive)).distinct
          val incompatible       =
            if (signatures.size <= 1) Nil
            else
              List(
                coordinate ->
                  s"[${coordinate.display}] Non-repeatable directive '@${definition.name}' has incompatible applications between subgraphs: ${SchemaComposition
                      .formatSources(values.map(_.source))}."
              )
          bySourceDuplicates ::: incompatible
        }
      }
    }
    val hidden                = sourceInfo.map { case (source, info) =>
      source -> (info.source.protocolDirectives ++ info.definitions.map(_.localName))
    }
    val diagnostics           = (sourceInfo.valuesIterator.flatMap(_.diagnostics).toList :::
      declarations.collect { case Left(error) => error } ::: nameCollisions ::: definitionDiagnostics :::
      applicationErrors).distinct.sorted

    new Result(
      hidden,
      selectedDefinitions.sortBy(value => value.definition.name -> value.source),
      mergedApplications,
      validApplications,
      deferredDiagnostics,
      diagnostics
    )
  }

  def schemaDirectives(document: Document): List[Directive] =
    document.schemaDefinition.toList.flatMap(_.directives) :::
      document.typeExtensions.collect { case extension: SchemaExtension => extension }.flatMap(_.directives)

  def linkedFeatures(document: Document): List[LinkedFeature] =
    schemaDirectives(document).filter(_.name == "link").flatMap { directive =>
      directive.arguments.get("url").collect { case StringValue(url) => url }.flatMap { url =>
        val normalized   = url.takeWhile(character => character != '?' && character != '#').stripSuffix("/")
        val versionStart = normalized.lastIndexOf('/')
        val identity     = if (versionStart < 0) "" else normalized.substring(0, versionStart)
        val nameStart    = identity.lastIndexOf('/')
        val name         = if (nameStart < 0) "" else identity.substring(nameStart + 1)
        val version      = if (versionStart < 0) "" else normalized.substring(versionStart + 1)

        version match {
          case VersionPattern(major, minor) if name.nonEmpty =>
            val imports   = directive.arguments.get("import").toList.flatMap {
              case InputListValue(values) => values.flatMap(importedName)
              case _                      => Nil
            }
            val namespace = directive.arguments
              .get("as")
              .collect { case StringValue(value) => value.stripPrefix("@") }
              .getOrElse(name)
            Some(
              LinkedFeature(
                directive,
                identity,
                name,
                FeatureVersion(major.toInt, minor.toInt),
                namespace,
                imports
              )
            )
          case _                                             => None
        }
      }
    }

  private[gateway] final case class ImportedName(name: String, alias: String, directive: Boolean)
  private final case class DirectiveKey(identity: String, member: String)
  private final case class LocalDefinition(
    source: String,
    localName: String,
    key: DirectiveKey,
    definition: __Directive
  )
  private final case class SelectedDefinition(source: String, key: DirectiveKey, definition: __Directive)
  private final case class RawApplication(
    source: String,
    key: DirectiveKey,
    coordinate: Coordinate,
    directive: Directive,
    definition: Option[__Directive],
    ordinal: Int
  )
  private final case class CompiledApplication(
    source: String,
    key: DirectiveKey,
    coordinate: Coordinate,
    directive: Directive,
    definition: __Directive,
    ordinal: Int
  )

  private final case class SourceInfo(source: Source) {
    private val schema                    = source.schema
    private val features                  = linkedFeatures(schema.document)
    private val interfaceObjectDirectives = features
      .filter(_.identity == FederationIdentity)
      .flatMap(_.directiveNames("interfaceObject"))
      .toSet
    private val byLocal                   = features.flatMap { feature =>
      schema.rootType.additionalDirectives.flatMap { definition =>
        feature
          .sourceDirective(definition.name)
          .map(member => definition.name -> DirectiveKey(feature.identity, member))
      }
    }.groupBy(_._1).map { case (name, values) => name -> values.map(_._2).distinct }
    private val localDefinitions          = schema.rootType.additionalDirectives.map { definition =>
      val keys = byLocal.getOrElse(definition.name, Nil)
      definition.name -> (keys match {
        case key :: Nil => key
        case _          => DirectiveKey("", definition.name)
      })
    }.toMap

    val definitions: List[LocalDefinition] = schema.rootType.additionalDirectives.map { definition =>
      LocalDefinition(schema.name, definition.name, localDefinitions(definition.name), definition)
    }

    val diagnostics: List[String] = byLocal.toList.collect {
      case (name, keys) if keys.size > 1 =>
        val identities = keys.map(_.identity).sorted.map(value => s"'$value'").mkString(" and ")
        s"[${schema.name}] Directive '@$name' resolves to multiple linked feature identities: $identities."
    }

    val ordinarySelections: Set[DirectiveKey] =
      if (schema.federation) Set.empty else definitions.map(_.key).toSet

    val composeDeclarations: List[Either[String, DirectiveKey]] = {
      val federation = features.filter(_.identity == FederationIdentity)
      schemaDirectives(schema.document).flatMap { directive =>
        val members = federation.flatMap(feature => feature.sourceDirective(directive.name).map(feature -> _))
        members.collectFirst { case (feature, "composeDirective") => feature }.toList.map { feature =>
          if (!feature.version.atLeast(2, 1))
            Left(
              s"[${schema.name}] Federation @composeDirective requires Federation v2.1 or newer."
            )
          else
            directive.arguments match {
              case arguments if arguments.keySet == Set("name") =>
                arguments("name") match {
                  case StringValue(value) if value.startsWith("@") && value.length > 1 =>
                    val localName = value.drop(1)
                    definitions.find(_.localName == localName) match {
                      case None             =>
                        Left(s"[${schema.name}] Composed directive '@$localName' is not defined by this subgraph.")
                      case Some(definition) =>
                        if (definition.key.identity.isEmpty)
                          Left(
                            s"[${schema.name}] Composed directive '@$localName' must be imported from a linked custom feature."
                          )
                        else if (
                          definition.key.identity == FederationIdentity && definition.key.member != "tag" ||
                          ReservedFeatureIdentities(definition.key.identity)
                        )
                          Left(s"[${schema.name}] Federation transport directive '@$localName' cannot be composed.")
                        else Right(definition.key)
                    }
                  case _                                                               =>
                    Left(
                      s"[${schema.name}] The composeDirective 'name' argument must start with '@' and name a directive."
                    )
                }
              case _                                            =>
                Left(s"[${schema.name}] Invalid Federation composeDirective application.")
            }
        }
      }
    }

    def applications(
      selected: Set[DirectiveKey],
      composedNames: Map[DirectiveKey, String]
    ): List[RawApplication] = {
      val rootNames = List(
        schema.rootType.queryType.name.map(_ -> "Query"),
        schema.rootType.mutationType.flatMap(_.name).map(_ -> "Mutation"),
        schema.rootType.subscriptionType.flatMap(_.name).map(_ -> "Subscription")
      ).flatten.groupBy(_._1).map { case (sourceName, values) => sourceName -> values.map(_._2) }

      def selectedDirectives(directives: Option[List[Directive]], coordinate: Coordinate): List[RawApplication] =
        directives.getOrElse(Nil).zipWithIndex.flatMap { case (directive, ordinal) =>
          localDefinitions.get(directive.name).toList.collect {
            case key if selected(key) =>
              RawApplication(
                schema.name,
                key,
                coordinate,
                directive.copy(name = composedNames.getOrElse(key, directive.name)),
                definitions.find(_.key == key).map(_.definition),
                ordinal
              )
          }
        }

      def inputApplications(
        coordinate: __InputValue => Coordinate,
        values: List[__InputValue]
      ): List[RawApplication] =
        values.flatMap(value => selectedDirectives(value.directives, coordinate(value)))

      val schemaApps            = selectedDirectives(Some(schemaDirectives(schema.document)), SchemaCoordinate)
      val typeApps              =
        schema.rootType.types.valuesIterator.toList.sortBy(_.name).flatMap(tpe => tpe.name.map(_ -> tpe)).flatMap {
          case (sourceName, tpe) =>
            rootNames.getOrElse(sourceName, sourceName :: Nil).flatMap { typeName =>
              val location   =
                if (
                  tpe.kind == __TypeKind.OBJECT &&
                  tpe.directives.exists(_.exists(directive => interfaceObjectDirectives(directive.name)))
                ) __DirectiveLocation.INTERFACE
                else typeLocation(tpe.kind)
              val coordinate = TypeCoordinate(typeName, location)
              selectedDirectives(tpe.directives, coordinate) :::
                tpe.allFields.flatMap { field =>
                  selectedDirectives(field.directives, FieldCoordinate(typeName, field.name)) :::
                    inputApplications(
                      argument => ArgumentCoordinate(typeName, field.name, argument.name),
                      field.allArgs
                    )
                } ::: inputApplications(field => InputFieldCoordinate(typeName, field.name), tpe.allInputFields) :::
                tpe.allEnumValues
                  .flatMap(value => selectedDirectives(value.directives, EnumValueCoordinate(typeName, value.name)))
            }
        }
      val directiveArgumentApps = schema.rootType.additionalDirectives.flatMap { definition =>
        inputApplications(
          argument =>
            DirectiveArgumentCoordinate(
              composedNames.getOrElse(localDefinitions(definition.name), definition.name),
              argument.name
            ),
          definition.allArgs
        )
      }

      schemaApps ::: typeApps ::: directiveArgumentApps
    }
  }

  private def compileApplication(application: RawApplication): Either[List[String], CompiledApplication] =
    application.definition match {
      case None             =>
        Left(
          List(
            s"[${application.source}] Directive '@${application.directive.name}' applied at '${application.coordinate.display}' is not defined by this subgraph."
          )
        )
      case Some(definition) =>
        val arguments      = definition.allArgs.map(argument => argument.name -> argument).toMap
        val unknown        = application.directive.arguments.keySet
          .diff(arguments.keySet)
          .toList
          .sorted
          .map(name =>
            s"[${application.source}] Unknown argument '$name' on composed directive '@${application.directive.name}' at '${application.coordinate.display}'."
          )
        val missing        = definition.allArgs.collect {
          case argument
              if argument._type.kind == __TypeKind.NON_NULL && argument.defaultValue.isEmpty &&
                !application.directive.arguments.contains(argument.name) =>
            s"[${application.source}] Required argument '${argument.name}' is missing on composed directive '@${application.directive.name}' at '${application.coordinate.display}'."
        }
        val location       =
          if (definition.locations(application.coordinate.location)) Nil
          else
            List(
              s"[${application.source}] Composed directive '@${application.directive.name}' does not support ${application.coordinate.location} at '${application.coordinate.display}'."
            )
        val valueErrors    = application.directive.arguments.toList.flatMap { case (name, value) =>
          arguments
            .get(name)
            .toList
            .flatMap(argument =>
              Validator
                .validateInputValues(
                  argument,
                  value,
                  Context.empty,
                  s"Argument '$name' of directive '@${application.directive.name}'"
                )
                .left
                .toOption
                .map(error => s"[${application.source}] ${error.getMessage}")
            )
        }
        val errors         = unknown ::: missing ::: location ::: valueErrors
        val normalizedArgs = definition.allArgs.flatMap { argument =>
          application.directive.arguments
            .get(argument.name)
            .orElse(defaultValue(argument))
            .map(value => argument.name -> canonicalValue(argument._type, value))
        }

        if (errors.nonEmpty) Left(errors)
        else
          Right(
            CompiledApplication(
              application.source,
              application.key,
              application.coordinate,
              application.directive.copy(arguments = ListMap(normalizedArgs: _*)),
              definition,
              application.ordinal
            )
          )
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
                .orElse(defaultValue(field))
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
      case __TypeKind.SCALAR if tpe.name.contains("ID")    =>
        value match {
          case number: IntValue => StringValue(number.toBigInt.toString)
          case other            => other
        }
      case _                                               => value
    }

  private def defaultValue(value: __InputValue): Option[InputValue] =
    value.defaultValue.flatMap(Parser.parseInputValue(_).toOption)

  private def canonicalDecimal(value: BigDecimal): BigDecimal =
    BigDecimal(value.bigDecimal.stripTrailingZeros)

  private def coordinateExists(rootType: RootType, coordinate: Coordinate): Boolean =
    coordinate match {
      case SchemaCoordinate                                         => true
      case TypeCoordinate(typeName, _)                              => rootType.types.contains(typeName)
      case FieldCoordinate(typeName, fieldName)                     =>
        rootType.types.get(typeName).exists(_.allFields.exists(_.name == fieldName))
      case ArgumentCoordinate(typeName, fieldName, argumentName)    =>
        rootType.types
          .get(typeName)
          .flatMap(_.allFields.find(_.name == fieldName))
          .exists(_.allArgs.exists(_.name == argumentName))
      case InputFieldCoordinate(typeName, fieldName)                =>
        rootType.types.get(typeName).exists(_.allInputFields.exists(_.name == fieldName))
      case EnumValueCoordinate(typeName, valueName)                 =>
        rootType.types.get(typeName).exists(_.allEnumValues.exists(_.name == valueName))
      case DirectiveArgumentCoordinate(directiveName, argumentName) =>
        rootType.additionalDirectives
          .find(_.name == directiveName)
          .exists(_.allArgs.exists(_.name == argumentName))
    }

  private def typeLocation(kind: __TypeKind): __DirectiveLocation =
    kind match {
      case __TypeKind.SCALAR       => __DirectiveLocation.SCALAR
      case __TypeKind.OBJECT       => __DirectiveLocation.OBJECT
      case __TypeKind.INTERFACE    => __DirectiveLocation.INTERFACE
      case __TypeKind.UNION        => __DirectiveLocation.UNION
      case __TypeKind.ENUM         => __DirectiveLocation.ENUM
      case __TypeKind.INPUT_OBJECT => __DirectiveLocation.INPUT_OBJECT
      case _                       => __DirectiveLocation.OBJECT
    }

  private final case class DefinitionArgumentSignature(
    name: String,
    typeName: String,
    defaultValue: Option[InputValue],
    deprecated: Boolean,
    deprecationReason: Option[String]
  )

  private final case class DefinitionSignature(
    repeatable: Boolean,
    locations: Set[__DirectiveLocation],
    arguments: List[DefinitionArgumentSignature]
  )

  private def definitionSignature(definition: __Directive): DefinitionSignature = {
    val arguments = definition.allArgs.sortBy(_.name).map { argument =>
      DefinitionArgumentSignature(
        argument.name,
        DocumentRenderer.renderTypeName(argument._type),
        defaultValue(argument).map(canonicalValue(argument._type, _)),
        argument.isDeprecated,
        argument.deprecationReason
      )
    }
    DefinitionSignature(definition.isRepeatable, definition.locations, arguments)
  }

  private def applicationSignature(directive: Directive): List[(String, InputValue)] =
    directive.arguments.toList.sortBy(_._1)

  private def importedName(value: InputValue): Option[ImportedName] =
    value match {
      case StringValue(name)        =>
        Some(ImportedName(name.stripPrefix("@"), name.stripPrefix("@"), name.startsWith("@")))
      case InputObjectValue(fields) =>
        fields.get("name").collect { case StringValue(name) => name }.map { name =>
          val alias = fields.get("as").collect { case StringValue(value) => value }.getOrElse(name)
          ImportedName(name.stripPrefix("@"), alias.stripPrefix("@"), name.startsWith("@"))
        }
      case _                        => None
    }

  private val VersionPattern            = "v([0-9]+)\\.([0-9]+)".r
  private val ReservedFeatureIdentities = Set(
    "https://specs.apollo.dev/link",
    "https://specs.apollo.dev/authenticated",
    "https://specs.apollo.dev/requiresScopes",
    "https://specs.apollo.dev/policy"
  )
}
