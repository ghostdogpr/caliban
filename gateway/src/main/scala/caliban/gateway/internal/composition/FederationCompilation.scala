package caliban.gateway.internal.composition

import caliban.gateway._
import caliban.gateway.CompositionDiagnostic.{ error, Code }
import caliban.gateway.internal.composition.ComposedGraph.KeyField
import caliban.gateway.internal.composition.DirectiveComposition._
import caliban.introspection.adt._
import caliban.parsing.adt.{ Directive, Document, Selection }
import caliban.schema.{ RootType, Types }

import scala.collection.compat._

private[gateway] object FederationCompilation {
  final case class FederationDirectiveNames(
    features: List[LinkedFeature],
    mode: SubgraphMode,
    resolved: Map[String, FederationDirective],
    unsupported: Map[String, UnsupportedDirective],
    hiddenTypes: Set[String]
  ) {
    def supportsProgressiveOverride: Boolean =
      features.exists(feature => feature.identity == FederationIdentity && feature.version.atLeast(2, 7))

    def is(directive: Directive, member: FederationDirective): Boolean = resolved.get(directive.name).contains(member)
  }

  sealed abstract class SubgraphMode(
    val federation: Boolean,
    // A field resolved by several subgraphs must then be a key field or declared @shareable.
    val strictSharing: Boolean,
    val bareDirectives: List[FederationDirective]
  )

  object SubgraphMode {
    // Introspection does not expose applied directives, so an introspected subgraph's are unknown.
    final case class Composite(directivesKnown: Boolean)
        extends SubgraphMode(federation = false, strictSharing = directivesKnown, FederationDirective.composite)
    case object Federation1
        extends SubgraphMode(federation = true, strictSharing = false, FederationDirective.federation1)
    case object Federation2 extends SubgraphMode(federation = true, strictSharing = true, Nil)
  }

  final case class UnsupportedDirective(code: Code, message: Coordinate => String) {
    def apply(source: String, at: Coordinate): CompositionDiagnostic =
      error(code, List(source), at.schemaCoordinate)(message(at))
  }

  /**
   * A directive defined by the Federation spec or a linked spec, resolved from its local name in a subgraph.
   */
  sealed abstract class FederationDirective(
    val name: String,
    locations: Set[__DirectiveLocation],
    arguments: List[__InputValue] = Nil,
    repeatable: Boolean = false,
    val federationVersion: FeatureVersion = FeatureVersion(0, 0),
    val specIdentity: Option[String] = None
  ) {
    lazy val definition: __Directive               = __Directive(name, None, locations, _ => arguments, repeatable)
    def allowedAt(coordinate: Coordinate): Boolean = locations(coordinate.location)
    def unavailableMessage: String                 = "is not available in the linked feature version"
  }

  object FederationDirective {
    import __DirectiveLocation._

    private val (v21, v25, v26, v28) =
      (FeatureVersion(2, 1), FeatureVersion(2, 5), FeatureVersion(2, 6), FeatureVersion(2, 8))
    private val Strings              = Types.string.nonNull.list
    private val Scopes               = Strings.nonNull.list.nonNull
    private val Secured              = Set[__DirectiveLocation](FIELD_DEFINITION, OBJECT, INTERFACE, SCALAR, ENUM)
    private val Everywhere           =
      Secured ++ Set(UNION, ARGUMENT_DEFINITION, ENUM_VALUE, INPUT_OBJECT, INPUT_FIELD_DEFINITION)

    private def required(name: String) = argument(name, Types.string.nonNull)
    private def optional(name: String) = argument(name, Types.string)
    private def flag(name: String)     = argument(name, Types.boolean, Some("true"))

    private def argument(name: String, tpe: __Type, default: Option[String] = None) =
      __InputValue(name, None, () => tpe, default)

    sealed abstract class Security(name: String, args: List[__InputValue], version: FeatureVersion, id: String)
        extends FederationDirective(name, Secured, args, false, version, Some(id))
    sealed abstract class CostSpec(name: String, locations: Set[__DirectiveLocation], args: List[__InputValue])
        extends FederationDirective(name, locations, args, false, FeatureVersion(2, 9), Some(CostIdentity)) {
      override def unavailableMessage: String = "requires Federation v2.9 or cost spec v0.1"
    }

    case object Key
        extends FederationDirective("key", Set(OBJECT, INTERFACE), List(required("fields"), flag("resolvable")), true)
    case object External
        extends FederationDirective("external", Set(OBJECT, FIELD_DEFINITION), List(optional("reason")))
    case object Extends         extends FederationDirective("extends", Set(OBJECT, INTERFACE))
    case object Shareable       extends FederationDirective("shareable", Set(OBJECT, FIELD_DEFINITION), Nil, true)
    case object Inaccessible    extends FederationDirective("inaccessible", Everywhere)
    case object Requires        extends FederationDirective("requires", Set(FIELD_DEFINITION), List(required("fields")))
    case object Provides        extends FederationDirective("provides", Set(FIELD_DEFINITION), List(required("fields")))
    case object InterfaceObject extends FederationDirective("interfaceObject", Set(OBJECT))
    case object Tag             extends FederationDirective("tag", Everywhere + SCHEMA, List(required("name")), true)
    case object ComposeDirective
        extends FederationDirective("composeDirective", Set(SCHEMA), List(required("name")), true, v21) {
      override def unavailableMessage: String = "requires Federation v2.1 or newer"
    }
    case object Override
        extends FederationDirective("override", Set(FIELD_DEFINITION), List(required("from"), optional("label")))
    case object LookupField extends FederationDirective("lookup", Set(FIELD_DEFINITION))
    case object Internal extends FederationDirective("internal", Set(OBJECT, FIELD_DEFINITION))
    case object Context
        extends FederationDirective("context", Set(OBJECT, INTERFACE, UNION), List(required("name")), true, v28)
    case object FromContext
        extends FederationDirective("fromContext", Set(ARGUMENT_DEFINITION), List(optional("field")), false, v28) {
      override def allowedAt(coordinate: Coordinate): Boolean = coordinate.isInstanceOf[ArgumentCoordinate]
    }
    case object Authenticated extends Security("authenticated", Nil, v25, AuthenticatedIdentity)
    case object RequiresScopes
        extends Security("requiresScopes", List(argument("scopes", Scopes)), v25, RequiresScopesIdentity)
    case object Policy extends Security("policy", List(argument("policies", Scopes)), v26, PolicyIdentity)
    case object Cost
        extends CostSpec(
          "cost",
          Secured - INTERFACE + ARGUMENT_DEFINITION + INPUT_FIELD_DEFINITION,
          List(argument("weight", Types.int.nonNull))
        )
    case object ListSize
        extends CostSpec(
          "listSize",
          Set(FIELD_DEFINITION),
          List(
            argument("assumedSize", Types.int),
            argument("slicingArguments", Strings),
            argument("sizedFields", Strings),
            flag("requireOneSlicingArgument")
          )
        )

    // Federation 1 subgraphs apply these without a @link.
    val federation1: List[FederationDirective] = List(Key, External, Extends, Requires, Provides)
    // Directives a Federation 2 subgraph imports, in the order a projected @link lists them.
    val imported: List[FederationDirective]    = List(Key, Shareable, External, Requires, Provides, Tag, Override) :::
      List(Inaccessible, InterfaceObject, Authenticated, RequiresScopes, Policy, Context, FromContext, Cost, ListSize)
    val all: List[FederationDirective]         = imported ::: List(Extends, ComposeDirective)

    // Composite source schemas apply these by their bare names.
    val composite: List[FederationDirective] =
      List(Key, External, Shareable, Inaccessible, Provides, Override, LookupField, Internal)
    val linkedSpecIdentities: Set[String]    = all.flatMap(_.specIdentity).toSet
  }

  final case class FederationApplication(coordinate: Coordinate, member: FederationDirective, directive: Directive)

  // Arguments are canonical: defaults are filled and single values are wrapped in lists.
  def validateApplication(
    source: String,
    coordinate: Coordinate,
    member: FederationDirective,
    directive: Directive
  ): Either[List[CompositionDiagnostic], FederationApplication] =
    if (!member.allowedAt(coordinate))
      Left(
        List(
          error(Code.InvalidGraphQL, List(source), coordinate.schemaCoordinate)(
            s"Federation @${member.name} is not supported at '${coordinate.display}'."
          )
        )
      )
    else
      validateArguments(source, s"Federation @${member.name}", coordinate, directive, member.definition)
        .map(FederationApplication(coordinate, member, _))

  def federationDirectiveNames(document: Document, mode: SubgraphMode): FederationDirectiveNames = {
    val links           = linkedFeatures(document)
    val federationLinks = links.filter(_.identity == FederationIdentity)
    val relevant        =
      links.filter(feature =>
        feature.identity == FederationIdentity || FederationDirective.linkedSpecIdentities(feature.identity)
      )
    val prefixes        = (relevant.map(_.namespace).toSet ++ (if (links.nonEmpty) Set("link") else Set.empty)).map(_ + "__")

    def isNamespaced(name: String): Boolean = prefixes.exists(name.startsWith)

    val linked                   = for {
      member  <- FederationDirective.all
      feature <- federationLinks ::: links.filter(feature => member.specIdentity.contains(feature.identity))
      name    <- feature.directiveNames(member.name)
    } yield {
      val available =
        if (feature.identity == FederationIdentity)
          feature.version.atLeast(member.federationVersion.major, member.federationVersion.minor)
        else feature.version == FeatureVersion(0, 1)
      Either.cond(available, name -> member, name -> member)
    }
    val (unavailable, available) = linked.partitionMap(identity)
    val resolved                 = (available ::: mode.bareDirectives.map(member => member.name -> member)).toMap
    val unresolved               = unavailable.toMap -- resolved.keySet
    val definedDirectives        = document.directiveDefinitions.iterator.map(_.name).toSet
    val recognizedSecurity       = (resolved ++ unresolved).collect { case (name, _: FederationDirective.Security) =>
      name
    }.toSet
    val unimportedSecurity       = FederationDirective.all.collect { case member: FederationDirective.Security =>
      member.name
    }.toSet
      .diff(recognizedSecurity)
      .diff(definedDirectives)
    val hiddenTypes              =
      document.typeDefinitions.iterator.map(_.name).filter(isNamespaced).toSet ++
        relevant.flatMap(_.imports).collect { case value if !value.isDirective => value.alias } ++
        Set(AnyType, "_Entity", "_FieldSet", ServiceType)

    FederationDirectiveNames(
      features = links,
      mode = mode,
      resolved = resolved,
      unsupported = unresolved.map { case (name, member) =>
        name -> UnsupportedDirective(
          Code.InvalidGraphQL,
          coordinate => s"Federation @${member.name} ${member.unavailableMessage} at '${coordinate.display}'."
        )
      } ++ (if (mode.federation) unimportedSecurity else Set.empty[String]).map { name =>
        name -> UnsupportedDirective(
          Code.UnenforceableSecurityDirective,
          coordinate =>
            s"Federation @$name at '${coordinate.display}' is not imported through a supported @link, so it would not be enforced."
        )
      },
      hiddenTypes = if (mode.federation) hiddenTypes else Set.empty
    )
  }

  /**
   * Composite subgraphs drop their directive definitions, and with them the types only those definitions use.
   */
  def directiveOnlyTypes(rootType: RootType): Set[String] = {
    val directiveTypes: Set[String] =
      Types.collectRootTypes(rootType.additionalDirectives.flatMap(_.allArgs.map(_._type)), Nil).flatMap(_.name).toSet
    val kept: Set[String]           =
      rootType.copy(additionalTypes = rootType.additionalTypes.filterNot(_.name.exists(directiveTypes))).types.keySet
    rootType.types.keySet -- kept
  }

  def plainFieldSet(selections: List[Selection]): Option[List[KeyField]] =
    traverseOption(selections) {
      case Selection.Field(None, name, arguments, directives, children, _) if arguments.isEmpty && directives.isEmpty =>
        plainFieldSet(children).map(KeyField(name, _))
      case _                                                                                                          => None
    }

  final case class TypeSystemDirectiveApplication(coordinate: Coordinate, directives: List[Directive])

  def definedTypes(rootType: RootType): List[__Type] =
    rootType.queryType :: rootType.mutationType.toList ::: rootType.subscriptionType.toList ::: rootType.additionalTypes

  def directiveApplications(
    rootType: RootType,
    schemaDirectives: List[Directive]
  ): List[TypeSystemDirectiveApplication] = {
    def application(coordinate: Coordinate, directives: Option[List[Directive]]): TypeSystemDirectiveApplication =
      TypeSystemDirectiveApplication(coordinate, directives.getOrElse(Nil))

    val types = definedTypes(rootType).flatMap { tpe =>
      val name = tpe.name.getOrElse("")
      application(TypeCoordinate(name, typeLocation(tpe.kind)), tpe.directives) ::
        tpe.allFields.flatMap { field =>
          application(FieldCoordinate(name, field.name), field.directives) :: field.allArgs.map(argument =>
            application(ArgumentCoordinate(name, field.name, argument.name), argument.directives)
          )
        } ::: tpe.allInputFields.map(field => application(InputFieldCoordinate(name, field.name), field.directives)) :::
        tpe.allEnumValues.map(value => application(EnumValueCoordinate(name, value.name), value.directives))
    }
    TypeSystemDirectiveApplication(SchemaDefinitionCoordinate, schemaDirectives) :: types :::
      rootType.additionalDirectives.flatMap(definition =>
        definition.allArgs.map(argument =>
          application(DirectiveArgumentCoordinate(definition.name, argument.name), argument.directives)
        )
      )
  }
}
