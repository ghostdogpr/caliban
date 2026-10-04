package caliban.gateway.internal.composition

import caliban.InputValue
import caliban.Value.{ BooleanValue, EnumValue, StringValue }
import caliban.gateway._
import caliban.gateway.internal.composition.DirectiveComposition.LinkedFeature
import caliban.gateway.internal.composition.FederationCompilation.FederationDirective
import caliban.parsing.adt.Definition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt.{ Directive, Document, Type }
import caliban.parsing.{ Parser, SourceMapper }
import zio.http.URL

/**
 * Decomposes an Apollo Federation supergraph into the subgraph documents it was composed from.
 *
 * Validates join metadata before projecting definitions into each subgraph document.
 */
private[gateway] object SupergraphDecomposition {

  /**
   * One entry of the join graph enum: a subgraph's identity and endpoint.
   */
  final case class Graph(key: String, name: String, url: URL)

  /**
   * A subgraph projected out of the supergraph.
   */
  final case class Projected(graph: Graph, document: Document)

  def decompose(document: Document): Either[List[String], List[Projected]] =
    (for {
      ctx        <- projectionContext(document)
      diagnostics = validate(ctx)
      projected  <- if (diagnostics.nonEmpty) Left(diagnostics)
                    else Right(ctx.registry.map(graph => Projected(graph, project(graph.key, ctx))))
    } yield projected).left.map(_.map(message => s"[supergraph] $message").distinct.sorted)

  private def graphs(document: Document, feature: LinkedFeature): Either[List[String], List[Graph]] =
    for {
      enumType <- graphEnum(document, feature).left.map(List(_))
      names     = feature.directiveNames("graph")
      results   = enumType.enumValuesDefinition.map(graphEntry(names, _))
      entries   = results.collect { case Right(graph) => graph }
      empty     = check(
                    enumType.enumValuesDefinition.nonEmpty,
                    s"The join graph enum '${enumType.name}' declares no subgraphs."
                  )
      repeated  =
        duplicates(entries.map(_.name)).map(name => s"Subgraph name '$name' is declared more than once.")
      graphs   <- validated(results.flatMap(_.left.getOrElse(Nil)) ::: empty ::: repeated, Right(entries))
    } yield graphs

  private def joinFeature(features: List[LinkedFeature]): Either[String, LinkedFeature] =
    features
      .find(_.identity == JoinIdentity)
      .toRight("The document does not @link the join feature and is not a supergraph.")
      .filterOrElse(
        _.version.atLeast(0, 2),
        "Supergraphs composed with Federation 1 (join/v0.1) are not supported."
      )

  private def graphEnum(document: Document, feature: LinkedFeature): Either[String, EnumTypeDefinition] = {
    val name = s"${feature.namespace}__Graph"
    document.enumTypeDefinitions
      .find(_.name == name)
      .toRight(s"The join graph enum '$name' is missing.")
  }

  private def graphEntry(names: Set[String], value: EnumValueDefinition): Either[List[String], Graph] =
    value.directives.find(directive => names.contains(directive.name)) match {
      case None            =>
        Left(List(s"Join graph '${value.enumValue}' has no graph directive."))
      case Some(directive) =>
        val prefix = s"Join graph '${value.enumValue}'"

        val name = stringArgument(directive.arguments, "name")
          .filter(_.trim.nonEmpty)
          .toRight(List(s"$prefix must declare a non-empty 'name' argument."))

        val url = stringArgument(directive.arguments, "url") match {
          case None        => Left(List(s"$prefix must declare a 'url' argument."))
          case Some(value) =>
            URL.decode(value).left.map(_ => List(s"$prefix must declare an absolute http or https 'url'."))
        }

        validated(name.left.getOrElse(Nil), url).flatMap(url => name.map(Graph(value.enumValue, _, url)))
    }

  private def projectionContext(document: Document): Either[List[String], ProjectionContext] = {
    val features = DirectiveComposition.linkedFeatures(document)
    for {
      feature  <- joinFeature(features).left.map(List(_))
      registry <- graphs(document, feature)
    } yield ProjectionContext(document, features, feature, registry)
  }

  private def boolean(directive: Directive, name: String, default: Boolean): Boolean =
    directive.arguments.get(name).collect { case BooleanValue(value) => value }.getOrElse(default)

  private def graphArgument(directive: Directive): Option[String] =
    directive.arguments.get("graph").collect { case EnumValue(value) => value }

  /**
   * Splits `<subgraph name>__<context name>`. The prefix is the original graph name, including
   * spaces, rather than its enum key. Graph names may contain `__`; context names cannot,
   * so split at the last separator.
   */
  private def contextOwner(name: String, ctx: ProjectionContext): Option[(Graph, String)] =
    name.lastIndexOf("__") match {
      case -1    => None
      case index =>
        val local = name.drop(index + 2)
        ctx.graphByName.get(name.take(index)).filter(_ => local.nonEmpty).map(_ -> local)
    }

  private def contextArguments(directive: Directive): List[JoinContextArgument] =
    directive.arguments.get("contextArguments").toList.flatMap(coercedList).flatMap {
      case InputValue.ObjectValue(fields) =>
        for {
          name        <- stringArgument(fields, "name")
          contextType <- stringArgument(fields, "type")
          context     <- stringArgument(fields, "context")
          selection   <- stringArgument(fields, "selection")
        } yield JoinContextArgument(name, contextType, context, selection)
      case _                              => None
    }

  private def declaredContextNames(directives: List[Directive], ctx: ProjectionContext): List[String] =
    directives
      .filter(directive => ctx.contextNames.contains(directive.name))
      .flatMap(directive => stringArgument(directive.arguments, "name"))

  /**
   * Decodes one `@join__field` application. Total: unreadable arguments read as absent.
   */
  private def joinField(directive: Directive): JoinField =
    JoinField(
      graph = graphArgument(directive),
      requires = stringArgument(directive.arguments, "requires"),
      provides = stringArgument(directive.arguments, "provides"),
      fieldType = stringArgument(directive.arguments, "type"),
      external = boolean(directive, "external", default = false),
      overrideFrom = stringArgument(directive.arguments, "override"),
      overrideLabel = stringArgument(directive.arguments, "overrideLabel"),
      contextArguments = contextArguments(directive),
      usedOverridden = boolean(directive, "usedOverridden", default = false)
    )

  /**
   * Decodes one `@join__type` application. `graph:` is non-null in the spec, so this can fail.
   */
  private def joinType(directive: Directive): Option[JoinType] =
    graphArgument(directive).map { graph =>
      JoinType(
        graph = graph,
        key = stringArgument(directive.arguments, "key"),
        resolvable = boolean(directive, "resolvable", default = true),
        isInterfaceObject = boolean(directive, "isInterfaceObject", default = false)
      )
    }

  /**
   * Graph keys that resolve the field of `entries`, with each graph's metadata when it declared any.
   *
   * A field with no `@join__field` belongs to every member graph. Otherwise only the named graphs
   * resolve it.
   */
  private def fieldGraphs(entries: List[JoinField], members: Set[String]): Map[String, JoinField] =
    if (entries.isEmpty) members.map(_ -> NoJoinField).toMap
    else entries.flatMap(entry => entry.graph.map(_ -> entry)).toMap

  private def parseFieldType(value: String): Either[String, Type] =
    Parser
      .parseQuery(s"type CalibanGatewayJoinProbe { field: $value }")
      .left
      .map(_ => value)
      .flatMap(
        _.objectTypeDefinitions.headOption
          .flatMap(_.fields.headOption)
          .map(_.ofType)
          .toRight(value)
      )

  private def validate(ctx: ProjectionContext): List[String] =
    check(
      ctx.document.typeExtensions.isEmpty,
      "A supergraph is fully composed and must not declare type extensions."
    ) ::: ctx.document.typeDefinitions.flatMap(typeDiagnostics(_, ctx))

  private def typeDiagnostics(definition: TypeDefinition, ctx: ProjectionContext): List[String] = {
    val entries  = ctx.join("type", definition.directives).map(joinType)
    val missing  = check(
      entries.forall(_.nonEmpty),
      s"Type '${definition.name}' has a join type entry without a 'graph' argument."
    )
    val unknown  = entries.flatten
      .map(_.graph)
      .filterNot(ctx.nameByKey.contains)
      .distinct
      .sorted
      .map(graph => s"Type '${definition.name}' names graph '$graph', which the graph enum omits.")
    // The declaring graph must receive the type for its context to survive projection.
    // Unrecognized namespaces are checked only when a field references them, allowing unused
    // declarations from composers with a different naming convention.
    val contexts = declaredContextNames(definition.directives, ctx)
      .flatMap(name => contextOwner(name, ctx).map(owner => name -> owner._1))
      .collect {
        case (name, graph) if !ctx.members(definition.name)(graph.key) =>
          s"Type '${definition.name}' declares context '$name' for graph '${graph.name}', " +
            "which does not declare the type."
      }

    missing ::: unknown ::: contexts ::: fieldDiagnostics(definition, ctx)
  }

  private def fieldDiagnostics(definition: TypeDefinition, ctx: ProjectionContext): List[String] = {
    val members = ctx.members(definition.name)
    val fields  = definition match {
      case value: AggregationTypeDefinition => value.fields
      case _                                => Nil
    }

    fields.flatMap { field =>
      val fieldName  = s"${definition.name}.${field.name}"
      val entries    = ctx.join("field", field.directives).map(joinField)
      val scoped     = entries.flatMap(_.graph)
      val unknown    = scoped
        .filterNot(members.contains)
        .distinct
        .sorted
        .map(graph => s"Field '$fieldName' names graph '$graph', which does not declare the type.")
      val repeated   =
        duplicates(scoped).map(graph => s"Field '$fieldName' declares more than one entry for graph '$graph'.")
      val types      = entries
        .flatMap(_.fieldType)
        .flatMap(parseFieldType(_).left.toOption)
        .map(value => s"Field '$fieldName' declares the unparseable type '$value'.")
      // Context arguments exist only in join metadata; invalid types cannot fall back to the field.
      val contextual = entries.flatMap { value =>
        value.contextArguments.map(value.graph.flatMap(ctx.nameByKey.get) -> _)
      }
      val argTypes   = contextual.flatMap { case (_, argument) => parseFieldType(argument.contextType).left.toOption }
        .map(value => s"Field '$fieldName' declares the unparseable context argument type '$value'.")
      // The selection is the half of the `@fromContext` argument the context name is prepended to,
      // so an empty one projects `@fromContext(field: "$viewer")`, which no subgraph can parse.
      val selections = contextual.collect {
        case (_, argument) if argument.selection.trim.isEmpty =>
          s"Field '$fieldName' declares an empty context argument selection " +
            s"for context '${argument.context}'."
      }
      val contexts   = contextual.flatMap { case (graph, argument) =>
        if (!ctx.declaredContexts.contains(argument.context))
          List(s"Field '$fieldName' names the undeclared context '${argument.context}'.")
        else {
          // Federation requires `@context` and `@fromContext` in the same subgraph, so an entry
          // naming another graph's context would project an argument no subgraph can resolve.
          val declaring = contextOwner(argument.context, ctx).map(_._1.name)
          graph
            .filterNot(declaring.contains)
            .map(name =>
              s"Field '$fieldName' names context '${argument.context}', " +
                s"which graph '$name' does not declare."
            )
            .toList
        }
      }

      unknown ::: repeated ::: types ::: argTypes ::: selections ::: contexts
    }
  }

  private def project(key: String, ctx: ProjectionContext): Document = {
    val definitions = ctx.document.definitions.flatMap {
      case definition: TypeDefinition      => projectType(definition, key, ctx).toList
      case definition: DirectiveDefinition => if (ctx.isFeatureDefinition(definition.name)) Nil else List(definition)
      case _                               => Nil
    }

    Document(projectSchema(definitions, key, ctx) :: definitions, SourceMapper.empty)
  }

  private def projectType(definition: TypeDefinition, key: String, ctx: ProjectionContext): Option[TypeDefinition] =
    if (ctx.isFeatureName(definition.name)) None
    else {
      val entries = ctx.join("type", definition.directives).flatMap(joinType).filter(_.graph == key)
      if (entries.isEmpty) None
      else {
        val members         = ctx.members(definition.name)
        val interfaceObject = entries.exists(_.isInterfaceObject)
        val directives      = projectDirectives(definition.directives, ctx) :::
          entries.flatMap(keyDirective) :::
          (if (interfaceObject) List(Directive("interfaceObject")) else Nil) :::
          contextDeclarations(definition.directives, key, ctx) :::
          composedDirectives(definition.directives, key, ctx)

        def listed(member: String, argument: String): Set[String] =
          ctx
            .join(member, definition.directives)
            .filter(graphArgument(_).contains(key))
            .flatMap { entry =>
              stringArgument(entry.arguments, argument)
            }
            .toSet
        lazy val implemented                                      = listed("implements", "interface")
        def sharing(field: String): Set[String]                   = ctx.sharingGraphs(definition, field, interfaceObject)

        val projected: TypeDefinition = definition match {
          case value: ObjectTypeDefinition                       =>
            value.copy(
              implements = value.implements.filter(interface => implemented(interface.name)),
              directives = directives,
              fields = projectFields(value.fields, members, key, ctx, sharing)
            )
          // The graph declared the supergraph interface as an object type.
          case value: InterfaceTypeDefinition if interfaceObject =>
            ObjectTypeDefinition(
              value.description,
              value.name,
              Nil,
              directives,
              projectFields(value.fields, members, key, ctx, sharing)
            )
          case value: InterfaceTypeDefinition                    =>
            value.copy(
              implements = value.implements.filter(interface => implemented(interface.name)),
              directives = directives,
              fields = projectFields(value.fields, members, key, ctx, sharing)
            )
          case value: UnionTypeDefinition                        =>
            // join/v0.2 has no @join__unionMember: a graph then keeps the members it defines.
            val kept: String => Boolean =
              if (ctx.join("unionMember", value.directives).isEmpty) ctx.members(_)(key)
              else listed("unionMember", "member")
            value.copy(directives = directives, memberTypes = value.memberTypes.filter(kept))
          case value: EnumTypeDefinition                         =>
            value.copy(
              directives = directives,
              enumValuesDefinition = projectEnumValues(value.enumValuesDefinition, key, ctx)
            )
          case value: InputObjectTypeDefinition                  =>
            value.copy(directives = directives, fields = projectInputFields(value.fields, key, ctx))
          case value: ScalarTypeDefinition                       =>
            value.copy(directives = directives)
        }

        Some(projected).filterNot(isEmpty)
      }
    }

  /**
   * Drop types with no projected members, such as an operation root with no fields in this graph.
   * GraphQL forbids empty composite types. Malformed input can still leave dangling references.
   */
  private def isEmpty(definition: TypeDefinition): Boolean = definition match {
    case value: AggregationTypeDefinition => value.fields.isEmpty
    case value: UnionTypeDefinition       => value.memberTypes.isEmpty
    case value: EnumTypeDefinition        => value.enumValuesDefinition.isEmpty
    case value: InputObjectTypeDefinition => value.fields.isEmpty
    case _: ScalarTypeDefinition          => false
  }

  private def projectDirectives(directives: List[Directive], ctx: ProjectionContext): List[Directive] =
    directives.flatMap { directive =>
      ctx.federationDirectiveName(directive.name) match {
        case Some(name) => List(directive.copy(name = name))
        case None       => if (ctx.isFeatureApplication(directive.name)) Nil else List(directive)
      }
    }

  private def keyDirective(entry: JoinType): Option[Directive] =
    entry.key.map { fields =>
      val arguments = Map[String, InputValue]("fields" -> StringValue(fields)) ++
        (if (entry.resolvable) Map.empty[String, InputValue] else Map("resolvable" -> BooleanValue(false)))
      Directive("key", arguments)
    }

  /**
   * Restores this graph's @context declarations under their original names. The namespace
   * identifies the owner when a type carries declarations from several graphs.
   */
  private def contextDeclarations(directives: List[Directive], key: String, ctx: ProjectionContext): List[Directive] =
    declaredContextNames(directives, ctx).flatMap(contextOwner(_, ctx)).collect {
      case (graph, local) if graph.key == key =>
        Directive("context", Map[String, InputValue]("name" -> StringValue(local)))
    }

  /**
   * Restores arguments removed from the supergraph field and recorded in join metadata.
   * Their original positions are unavailable, so append them. SchemaComposer hides these
   * arguments from clients when composing the projected subgraphs.
   */
  private def contextArgumentDefinitions(
    entry: JoinField,
    key: String,
    ctx: ProjectionContext
  ): List[InputValueDefinition] =
    entry.contextArguments.flatMap { argument =>
      contextOwner(argument.context, ctx).collect {
        case (graph, local) if graph.key == key =>
          InputValueDefinition(
            description = None,
            name = argument.name,
            // `validate` already proved every declared type parses, so the fallback is unreachable.
            ofType =
              parseFieldType(argument.contextType).toOption.getOrElse(NamedType(argument.contextType, nonNull = false)),
            defaultValue = None,
            directives = List(
              Directive(
                "fromContext",
                Map[String, InputValue]("field" -> StringValue(s"$$$local ${argument.selection.trim}"))
              )
            )
          )
      }
    }

  /**
   * Re-emits `@join__directive(graphs:, name:, args:)` as the directive it stands for.
   */
  private def composedDirectives(directives: List[Directive], key: String, ctx: ProjectionContext): List[Directive] =
    ctx.join("directive", directives).flatMap { directive =>
      val graphs =
        directive.arguments.get("graphs").toList.flatMap(coercedList).collect { case EnumValue(value) => value }
      val args   = directive.arguments
        .get("args")
        .collect { case InputValue.ObjectValue(fields) => fields }
        .getOrElse(Map.empty[String, InputValue])

      if (graphs.contains(key))
        stringArgument(directive.arguments, "name").map(name => Directive(name.stripPrefix("@"), args)).toList
      else Nil
    }

  private def projectFields(
    fields: List[FieldDefinition],
    members: Set[String],
    key: String,
    ctx: ProjectionContext,
    sharing: String => Set[String]
  ): List[FieldDefinition] =
    fields.flatMap { field =>
      val owners = fieldGraphs(ctx.join("field", field.directives).map(joinField), members)
      owners.get(key).map { entry =>
        projectField(field, entry, key, ctx, shareable = resolves(entry) && sharing(field.name).size > 1)
      }
    }

  /**
   * True when a graph actually resolves the field, rather than merely declaring it.
   */
  private def resolves(entry: JoinField): Boolean =
    !entry.external && !entry.usedOverridden

  /**
   * Graphs that actually resolve the field: declared owners, minus the ones that only declare it,
   * minus any graph another graph has overridden away.
   */
  private def resolvingGraphs(owners: Map[String, JoinField], ctx: ProjectionContext): Set[String] = {
    val overridden = owners.valuesIterator.flatMap(_.overrideFrom).flatMap(ctx.graphByName.get).map(_.key).toSet
    owners.collect { case (graph, entry) if resolves(entry) && !overridden.contains(graph) => graph }.toSet
  }

  private def projectField(
    field: FieldDefinition,
    entry: JoinField,
    key: String,
    ctx: ProjectionContext,
    shareable: Boolean
  ): FieldDefinition = {
    val translated = List(
      entry.requires.map(fields => Directive("requires", Map("fields" -> StringValue(fields)))),
      entry.provides.map(fields => Directive("provides", Map("fields" -> StringValue(fields)))),
      Some(Directive("external")).filter(_ => entry.external || entry.usedOverridden && entry.overrideLabel.isEmpty),
      entry.overrideFrom.map(from =>
        Directive("override", Map("from" -> StringValue(from)) ++ entry.overrideLabel.map("label" -> StringValue(_)))
      ),
      // Outside the entry: a field with no join entry at all is the default-ownership case, which
      // lands in every member graph and so is the one that most needs declaring shareable.
      Some(Directive("shareable")).filter(_ => shareable)
    ).flatten
    // `validate` already proved every declared type parses, so the fallback is unreachable.
    val ofType     = entry.fieldType.flatMap(parseFieldType(_).toOption).getOrElse(field.ofType)

    field.copy(
      ofType = ofType,
      directives =
        projectDirectives(field.directives, ctx) ::: translated ::: composedDirectives(field.directives, key, ctx),
      args = field.args.map(argument => argument.copy(directives = projectDirectives(argument.directives, ctx))) :::
        contextArgumentDefinitions(entry, key, ctx)
    )
  }

  private def projectEnumValues(values: List[EnumValueDefinition], key: String, ctx: ProjectionContext) =
    values.collect {
      case value if ctx.inGraph("enumValue", value.directives, key) =>
        value.copy(directives = projectDirectives(value.directives, ctx))
    }

  private def projectInputFields(fields: List[InputValueDefinition], key: String, ctx: ProjectionContext) =
    fields.collect {
      case field if ctx.inGraph("field", field.directives, key) =>
        field.copy(directives = projectDirectives(field.directives, ctx))
    }

  /**
   * Names an operation root only when its type survived projection with at least one field.
   */
  private def projectSchema(
    definitions: List[Definition],
    key: String,
    ctx: ProjectionContext
  ): SchemaDefinition = {
    val populated = definitions.collect {
      case value: ObjectTypeDefinition if value.fields.nonEmpty => value.name
    }.toSet
    val declared  = ctx.document.schemaDefinition

    def root(name: Option[String], fallback: String): Option[String] =
      name.orElse(Some(fallback)).filter(populated.contains)

    val query        = root(declared.flatMap(_.query), "Query")
    val mutation     = root(declared.flatMap(_.mutation), "Mutation")
    val subscription = root(declared.flatMap(_.subscription), "Subscription")
    val directives   = composedDirectives(DirectiveComposition.schemaDirectives(ctx.document), key, ctx)

    // Even a graph without roots needs the federation link so SchemaComposer recognizes its
    // entity keys and routing directives.
    SchemaDefinition(FederationLink :: directives, query, mutation, subscription, None)
  }

  private final case class ProjectionContext(
    document: Document,
    features: List[LinkedFeature],
    feature: LinkedFeature,
    registry: List[Graph]
  ) {
    val graphByName: Map[String, Graph] = registry.map(graph => graph.name -> graph).toMap
    val nameByKey: Map[String, String]  = registry.map(graph => graph.key -> graph.name).toMap
    // `@context` is applied by the supergraph itself rather than by the join feature, so its
    // names come from the context feature and are empty for a supergraph that declares none.
    val contextNames: Set[String]       =
      features.filter(_.identity == ContextIdentity).flatMap(_.directiveNames("context")).toSet

    /**
     * Every `@context` name the supergraph declares, still namespaced as the composer wrote it.
     */
    lazy val declaredContexts: Set[String] =
      document.typeDefinitions.flatMap(definition => declaredContextNames(definition.directives, this)).toSet
    private val prefixes                   = features.map(_.namespace).toSet.+("link").map(_ + "__")
    private val linkNames                  =
      features.filter(_.identity == LinkIdentity).flatMap(_.directiveNames("link")).toSet + "link"
    private val claimedDefinitions         = document.directiveDefinitions.iterator
      .map(_.name)
      .filter(name => features.exists(_.sourceDirective(name).isDefined))
      .toSet
    private val projectedFeatures          = features.flatMap(f => ProjectedFeatures.get(f.identity).map(f -> _))

    def join(member: String, directives: List[Directive]): List[Directive] = {
      val names = feature.directiveNames(member)
      directives.filter(directive => names(directive.name))
    }

    private lazy val typeGraphs: Map[String, Set[String]] =
      document.typeDefinitions.map(d => d.name -> join("type", d.directives).flatMap(graphArgument).toSet).toMap

    /**
     * Graph keys a type belongs to.
     */
    def members(typeName: String): Set[String] = typeGraphs.getOrElse(typeName, Set.empty)

    private lazy val fieldResolvers: Map[(String, String), Set[String]] =
      document.typeDefinitions.collect { case value: AggregationTypeDefinition => value }.flatMap { value =>
        value.fields.map { field =>
          (value.name, field.name) ->
            resolvingGraphs(fieldGraphs(join("field", field.directives).map(joinField), members(value.name)), this)
        }
      }.toMap

    private lazy val interfaceObjectGraphs: Map[String, Set[String]] =
      document.typeDefinitions
        .map(d => d.name -> join("type", d.directives).flatMap(joinType).filter(_.isInterfaceObject).map(_.graph).toSet)
        .toMap

    private lazy val implementations: Map[String, List[String]] =
      document.objectTypeDefinitions.flatMap(value => value.implements.map(_.name -> value.name)).groupMap(_._1)(_._2)

    /**
     * Graph keys resolving the field, counting an interface object and the implementations it stands in for as one
     * field.
     */
    def sharingGraphs(definition: TypeDefinition, field: String, interfaceObject: Boolean): Set[String] = {
      def resolving(typeName: String) = fieldResolvers.getOrElse(typeName -> field, Set.empty[String])
      val standIns                    = definition match {
        case value: ObjectTypeDefinition =>
          value.implements.flatMap(i => resolving(i.name).intersect(interfaceObjectGraphs.getOrElse(i.name, Set.empty)))
        case _ if interfaceObject        => implementations.getOrElse(definition.name, Nil).flatMap(resolving)
        case _                           => Nil
      }
      resolving(definition.name) ++ standIns
    }

    def inGraph(member: String, directives: List[Directive], key: String): Boolean = {
      val entries = join(member, directives)
      entries.isEmpty || entries.exists(graphArgument(_).contains(key))
    }

    /**
     * A type or directive name owned by a linked feature, so absent from subgraph output.
     */
    def isFeatureName(name: String): Boolean = prefixes.exists(name.startsWith)

    /**
     * Removes join/link metadata after supported feature applications have been translated by
     * [[projectDirectives]]. Namespaced @context declarations are restored separately for their
     * owning graph by [[contextDeclarations]].
     */
    def isFeatureApplication(name: String): Boolean =
      feature.sourceDirective(name).isDefined || linkNames.contains(name) || isFeatureName(name) ||
        contextNames.contains(name)

    /**
     * Directive definitions removed from subgraph output: anything a linked feature supplies.
     */
    def isFeatureDefinition(name: String): Boolean = claimedDefinitions.contains(name) || isFeatureName(name)

    def federationDirectiveName(name: String): Option[String] =
      projectedFeatures.iterator.map { case (feature, members) =>
        feature.sourceDirective(name).filter(members)
      }.collectFirst { case Some(name) => name }
  }

  private final case class JoinType(graph: String, key: Option[String], resolvable: Boolean, isInterfaceObject: Boolean)

  private final case class JoinField(
    graph: Option[String],
    requires: Option[String],
    provides: Option[String],
    fieldType: Option[String],
    external: Boolean,
    overrideFrom: Option[String],
    overrideLabel: Option[String],
    contextArguments: List[JoinContextArgument],
    usedOverridden: Boolean
  )

  private val NoJoinField = joinField(Directive("field"))

  private final case class JoinContextArgument(name: String, contextType: String, context: String, selection: String)

  private val ProjectedFeatures = Map(
    FederationIdentity   -> FederationDirective.imported.map(_.name).toSet.diff(Set("context", "fromContext")),
    InaccessibleIdentity -> Set("inaccessible"),
    TagIdentity          -> Set("tag")
  ) ++ FederationDirective.imported
    .flatMap(member => member.specIdentity.map(_ -> member.name))
    .groupBy(_._1)
    .map { case (identity, members) => identity -> members.map(_._2).toSet }
  private val FederationLink    = Directive(
    "link",
    Map[String, InputValue](
      "url"    -> StringValue(s"$FederationIdentity/v2.9"),
      "import" -> InputValue.ListValue(FederationDirective.imported.map(member => StringValue(s"@${member.name}")))
    )
  )
}
