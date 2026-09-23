package caliban.gateway.internal.composition

import caliban.gateway._
import caliban.gateway.internal.composition.DirectiveComposition._
import caliban.introspection.adt._
import caliban.parsing.adt.{ Directive, Document }
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._

private[composition] object FederationCompilation {
  final case class FederationDirectiveNames(
    features: List[LinkedFeature],
    federation2: Boolean,
    key: Set[String],
    external: Set[String],
    extendsDirective: Set[String],
    shareable: Set[String],
    inaccessible: Set[String],
    overrideDirective: Set[String],
    requires: Set[String],
    provides: Set[String],
    interfaceObject: Set[String],
    authenticated: Set[String],
    requiresScopes: Set[String],
    policy: Set[String],
    unavailableSecurity: Map[String, String],
    unimportedSecurity: Set[String],
    unavailableCost: Map[String, String],
    cost: Set[String],
    listSize: Set[String],
    context: Set[String],
    fromContext: Set[String],
    supportsContexts: Boolean,
    supportsProgressiveOverride: Boolean,
    hidden: Set[String],
    hiddenTypes: Set[String]
  ) {
    val fieldSetDirectives: Set[String] = key ++ requires ++ provides
  }

  def federationDirectiveNames(document: Document): FederationDirectiveNames = {
    val links      = linkedFeatures(document)
    val federation = links.filter(_.identity == FederationIdentity)
    val relevant   =
      links.filter(feature => feature.identity == FederationIdentity || LinkedSpecIdentities(feature.identity))
    val prefixes   = (relevant.map(_.namespace).toSet ++ (if (links.nonEmpty) Set("link") else Set.empty)).map(_ + "__")

    def isNamespaced(name: String): Boolean = prefixes.exists(name.startsWith)

    def federationNames(name: String): Set[String] = federation.flatMap(_.directiveNames(name)).toSet

    def federation1Names(name: String): Set[String] = if (federation.isEmpty) Set(name) else federationNames(name)

    def specNames(name: String, identity: String, federationVersion: FeatureVersion): (Set[String], Set[String]) = {
      val (available, unavailable) = (federation ::: links.filter(_.identity == identity)).partition { feature =>
        if (feature.identity == FederationIdentity)
          feature.version.atLeast(federationVersion.major, federationVersion.minor)
        else feature.version == FeatureVersion(0, 1)
      }
      available.flatMap(_.directiveNames(name)).toSet -> unavailable.flatMap(_.directiveNames(name)).toSet
    }

    val keyNames                                    = federation1Names("key")
    val externalNames                               = federation1Names("external")
    val extendsNames                                = federation1Names("extends")
    val shareableNames                              = federationNames("shareable")
    val inaccessibleNames                           = federationNames("inaccessible")
    val overrideNames                               = federationNames("override")
    val requiresNames                               = federation1Names("requires")
    val providesNames                               = federation1Names("provides")
    val interfaceObjectNames                        = federationNames("interfaceObject")
    val (authenticated, unavailableAuthenticated)   =
      specNames("authenticated", AuthenticatedIdentity, FeatureVersion(2, 5))
    val (requiresScopes, unavailableRequiresScopes) =
      specNames("requiresScopes", RequiresScopesIdentity, FeatureVersion(2, 5))
    val (policy, unavailablePolicy)                 = specNames("policy", PolicyIdentity, FeatureVersion(2, 6))
    val (cost, unavailableCostNames)                = specNames("cost", CostIdentity, FeatureVersion(2, 9))
    val (listSize, unavailableListSizeNames)        = specNames("listSize", CostIdentity, FeatureVersion(2, 9))
    val unavailableSecurity                         =
      unavailableAuthenticated.map(_ -> "@authenticated").toMap ++
        unavailableRequiresScopes.map(_ -> "@requiresScopes").toMap ++
        unavailablePolicy.map(_ -> "@policy").toMap
    val recognizedSecurity                          = authenticated ++ requiresScopes ++ policy ++ unavailableSecurity.keySet
    val definedDirectives                           = document.directiveDefinitions.iterator.map(_.name).toSet
    val unimportedSecurity                          =
      Set("authenticated", "requiresScopes", "policy").diff(recognizedSecurity).diff(definedDirectives)
    val unavailableCost                             =
      unavailableCostNames.map(_ -> "@cost").toMap ++ unavailableListSizeNames.map(_ -> "@listSize").toMap
    val context                                     = federationNames("context")
    val fromContext                                 = federationNames("fromContext")
    val hidden                                      =
      Set("link") ++ keyNames ++ externalNames ++ extendsNames ++ shareableNames ++ inaccessibleNames ++
        overrideNames ++ requiresNames ++ providesNames ++ interfaceObjectNames ++ federationNames("tag") ++
        federationNames("composeDirective") ++ authenticated ++ requiresScopes ++ policy ++
        unavailableSecurity.keySet ++ unavailableCost.keySet ++ cost ++ listSize ++ context ++ fromContext ++
        document.directiveDefinitions.iterator.map(_.name).filter(isNamespaced)
    val hiddenTypes                                 =
      document.typeDefinitions.iterator.map(_.name).filter(isNamespaced).toSet ++
        relevant.flatMap(_.imports).collect { case value if !value.isDirective => value.alias } ++
        Set(AnyType, "_Entity", "_FieldSet", ServiceType)

    FederationDirectiveNames(
      features = links,
      federation2 = federation.nonEmpty,
      key = keyNames,
      external = externalNames,
      extendsDirective = extendsNames,
      shareable = shareableNames,
      inaccessible = inaccessibleNames,
      overrideDirective = overrideNames,
      requires = requiresNames,
      provides = providesNames,
      interfaceObject = interfaceObjectNames,
      authenticated = authenticated,
      requiresScopes = requiresScopes,
      policy = policy,
      unavailableSecurity = unavailableSecurity,
      unimportedSecurity = unimportedSecurity,
      unavailableCost = unavailableCost,
      cost = cost,
      listSize = listSize,
      context = context,
      fromContext = fromContext,
      supportsContexts = federation.exists(_.version.atLeast(2, 8)),
      supportsProgressiveOverride = federation.exists(_.version.atLeast(2, 7)),
      hidden = hidden,
      hiddenTypes = hiddenTypes
    )
  }

  def federationTransportTypes(document: Document, federation: Boolean): Set[String] =
    if (federation) federationDirectiveNames(document).hiddenTypes else Set.empty

  private val LinkedSpecIdentities = Set(AuthenticatedIdentity, RequiresScopesIdentity, PolicyIdentity, CostIdentity)

  final case class TypeSystemDirectiveApplication(coordinate: Coordinate, directives: List[Directive]) {
    def securityCoordinate: Option[(String, Option[String])] =
      coordinate match {
        case TypeCoordinate(typeName, location)
            if location == __DirectiveLocation.SCALAR || location == __DirectiveLocation.OBJECT ||
              location == __DirectiveLocation.INTERFACE || location == __DirectiveLocation.ENUM =>
          Some(typeName -> None)
        case FieldCoordinate(typeName, fieldName) => Some(typeName -> Some(fieldName))
        case _                                    => None
      }

    def supportsOverride: Boolean = coordinate.isInstanceOf[FieldCoordinate]

    def supportsContext: Boolean =
      coordinate match {
        case TypeCoordinate(_, location) =>
          location == __DirectiveLocation.OBJECT || location == __DirectiveLocation.INTERFACE ||
          location == __DirectiveLocation.UNION
        case _                           => false
      }

    def supportsFromContext: Boolean = coordinate.isInstanceOf[ArgumentCoordinate]
  }

  /**
   * Schema syntax inspected once and shared by directive, context and entity-key compilation.
   */
  final class SchemaInspection(document: Document) {
    val objectLikeTypes                    = document.typeDefinitions.collect { case value: AggregationTypeDefinition => value }
    val fields                             = objectLikeTypes.flatMap(tpe => tpe.fields.map(tpe.name -> _))
    private val unions                     = document.typeDefinitions.collect { case value: UnionTypeDefinition => value }
    val contextTypes: List[TypeDefinition] = objectLikeTypes ::: unions

    def directiveApplications(composedName: String => String): List[TypeSystemDirectiveApplication] = {
      def application(coordinate: Coordinate, directives: List[Directive]) =
        TypeSystemDirectiveApplication(coordinate, directives)

      // Preserve the diagnostic/application order used by composition.
      val types        = document.typeDefinitions.collect { case value: ScalarTypeDefinition => value } :::
        contextTypes :::
        document.typeDefinitions.collect { case value: EnumTypeDefinition => value } :::
        document.typeDefinitions.collect { case value: InputObjectTypeDefinition => value }
      val applications = types.flatMap { tpe =>
        val name                 = composedName(tpe.name)
        val (location, children) = tpe match {
          case value: AggregationTypeDefinition =>
            val location =
              if (value.isInstanceOf[ObjectTypeDefinition]) __DirectiveLocation.OBJECT
              else __DirectiveLocation.INTERFACE
            location -> value.fields.flatMap { field =>
              application(FieldCoordinate(name, field.name), field.directives) :: field.args.map(argument =>
                application(ArgumentCoordinate(name, field.name, argument.name), argument.directives)
              )
            }
          case value: EnumTypeDefinition        =>
            __DirectiveLocation.ENUM -> value.enumValuesDefinition.map(value =>
              application(EnumValueCoordinate(name, value.enumValue), value.directives)
            )
          case value: InputObjectTypeDefinition =>
            __DirectiveLocation.INPUT_OBJECT -> value.fields.map(field =>
              application(InputFieldCoordinate(name, field.name), field.directives)
            )
          case _: ScalarTypeDefinition          => __DirectiveLocation.SCALAR -> Nil
          case _: UnionTypeDefinition           => __DirectiveLocation.UNION  -> Nil
        }
        application(TypeCoordinate(name, location), tpe.directives) :: children
      }
      document.schemaDefinition.toList.map(value => application(SchemaCoordinate, value.directives)) :::
        applications ::: document.directiveDefinitions.flatMap(definition =>
          definition.args.map(argument =>
            application(DirectiveArgumentCoordinate(definition.name, argument.name), argument.directives)
          )
        )
    }
  }
}
