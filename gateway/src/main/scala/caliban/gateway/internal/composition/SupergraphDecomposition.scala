package caliban.gateway.internal.composition

import caliban.InputValue
import caliban.Value.{ BooleanValue, EnumValue, StringValue }
import caliban.gateway.internal.composition.DirectiveComposition.LinkedFeature
import caliban.parsing.adt.Definition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.{ DirectiveDefinition, SchemaDefinition, TypeDefinition }
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt.{ Directive, Document, Type }
import caliban.parsing.{ Parser, SourceMapper }
import zio.http.{ Scheme, URL }

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
    for {
      ctx        <- projectionContext(document)
      diagnostics = validate(document, ctx)
      projected  <- if (diagnostics.nonEmpty) Left(diagnostics)
                    else Right(ctx.registry.map(graph => Projected(graph, project(document, graph.key, ctx))))
    } yield projected

  def graphs(document: Document): Either[List[String], List[Graph]] =
    joinFeature(DirectiveComposition.linkedFeatures(document)).left.map(List(_)).flatMap(graphs(document, _))

  private def graphs(document: Document, feature: LinkedFeature): Either[List[String], List[Graph]] =
    for {
      enumType   <- graphEnum(document, feature).left.map(List(_))
      names       = feature.directiveNames("graph")
      results     = enumType.enumValuesDefinition.map(graphEntry(names, _))
      failures    = results.flatMap(_.left.getOrElse(Nil))
      entries     = results.collect { case Right(graph) => graph }
      empty       = if (enumType.enumValuesDefinition.isEmpty)
                      List(s"[supergraph] The join graph enum '${enumType.name}' declares no subgraphs.")
                    else Nil
      duplicates  = entries
                      .groupBy(_.name)
                      .collect { case (name, _ :: _ :: _) =>
                        s"[supergraph] Subgraph name '$name' is declared more than once."
                      }
                      .toList
      diagnostics = (failures ::: empty ::: duplicates).distinct.sorted
      graphs     <- if (diagnostics.nonEmpty) Left(diagnostics) else Right(entries)
    } yield graphs

  private def joinFeature(features: List[LinkedFeature]): Either[String, LinkedFeature] =
    features
      .find(_.identity == JoinIdentity)
      .toRight("[supergraph] The document does not @link the join feature and is not a supergraph.")

  private def graphEnum(document: Document, feature: LinkedFeature): Either[String, EnumTypeDefinition] = {
    val name = s"${feature.namespace}__Graph"
    document.enumTypeDefinitions
      .find(_.name == name)
      .toRight(s"[supergraph] The join graph enum '$name' is missing.")
  }

  private def graphEntry(names: Set[String], value: EnumValueDefinition): Either[List[String], Graph] =
    value.directives.find(directive => names.contains(directive.name)) match {
      case None            =>
        Left(List(s"[supergraph] Join graph '${value.enumValue}' has no graph directive."))
      case Some(directive) =>
        val prefix = s"[supergraph] Join graph '${value.enumValue}'"

        val name = string(directive, "name")
          .filter(_.trim.nonEmpty)
          .toRight(List(s"$prefix must declare a non-empty 'name' argument."))

        // Parsing alone is not validation: "foo" decodes into a relative URL. Require an absolute
        // http(s) endpoint rather than let an unusable URL fail later at request time.
        val url = string(directive, "url") match {
          case None        => Left(List(s"$prefix must declare a 'url' argument."))
          case Some(value) =>
            URL
              .decode(value)
              .toOption
              .filter(isHttpEndpoint)
              .toRight(List(s"$prefix must declare an absolute http or https 'url'."))
        }

        (name, url) match {
          case (Right(name), Right(url)) => Right(Graph(value.enumValue, name, url))
          case (name, url)               => Left(name.left.getOrElse(Nil) ::: url.left.getOrElse(Nil))
        }
    }

  private def isHttpEndpoint(url: URL): Boolean =
    url.scheme.exists(scheme => scheme == Scheme.HTTP || scheme == Scheme.HTTPS) && url.host.exists(_.nonEmpty)

  private def projectionContext(document: Document): Either[List[String], ProjectionContext] = {
    val features = DirectiveComposition.linkedFeatures(document)
    for {
      feature  <- joinFeature(features).left.map(List(_))
      registry <- graphs(document, feature)
    } yield {
      val prefixes         = features.map(_.namespace).toSet.+("link").map(_ + "__")
      val linkNames        = features.filter(_.identity == LinkIdentity).flatMap(_.directiveNames("link")).toSet + "link"
      val claimed          = document.directiveDefinitions.iterator
        .map(_.name)
        .filter(name => features.exists(_.sourceDirective(name).isDefined))
        .toSet
      val subscriptionRoot = document.schemaDefinition.flatMap(_.subscription)
      // `@context` is applied by the supergraph itself rather than by the join feature, so its
      // names come from the context feature and are empty for a supergraph that declares none.
      val contexts         = features.filter(_.identity == ContextIdentity).flatMap(_.directiveNames("context")).toSet

      ProjectionContext(
        feature = feature,
        names = joinNames(feature),
        registry = registry,
        prefixes = prefixes,
        linkNames = linkNames,
        claimedDefinitions = claimed,
        subscriptionRoot = subscriptionRoot,
        contextNames = contexts,
        projectedFeatures = features.filter(feature => ProjectedFeatures.contains(feature.identity))
      )
    }
  }

  private def joinNames(feature: LinkedFeature): JoinNames =
    JoinNames(
      tpe = feature.directiveNames("type"),
      field = feature.directiveNames("field"),
      implements = feature.directiveNames("implements"),
      unionMember = feature.directiveNames("unionMember"),
      enumValue = feature.directiveNames("enumValue"),
      directive = feature.directiveNames("directive")
    )

  private def string(directive: Directive, name: String): Option[String] =
    directive.arguments.get(name).collect { case StringValue(value) => value }

  private def boolean(directive: Directive, name: String, default: Boolean): Boolean =
    directive.arguments.get(name).collect { case BooleanValue(value) => value }.getOrElse(default)

  private def graphArgument(directive: Directive): Option[String] =
    directive.arguments.get("graph").collect { case EnumValue(value) => value }

  /**
   * Splits `<subgraph name>__<context name>`. The prefix is the original graph name, including
   * spaces, rather than its enum key. Graph names may contain `__`; context names cannot,
   * so split at the last separator.
   */
  private def contextOwner(name: String, ctx: ProjectionContext): Option[(String, String)] =
    name.lastIndexOf("__") match {
      case -1    => None
      case index =>
        val graph = name.take(index)
        val local = name.drop(index + 2)
        if (local.nonEmpty && ctx.graphNames.contains(graph)) Some(graph -> local) else None
    }

  private def contextArguments(directive: Directive): Option[List[ContextArgument]] =
    directive.arguments.get("contextArguments").collect { case InputValue.ListValue(values) =>
      values.collect { case InputValue.ObjectValue(fields) =>
        for {
          name        <- fields.get("name").collect { case StringValue(value) => value }
          contextType <- fields.get("type").collect { case StringValue(value) => value }
          context     <- fields.get("context").collect { case StringValue(value) => value }
          selection   <- fields.get("selection").collect { case StringValue(value) => value }
        } yield ContextArgument(name, contextType, context, selection)
      }.flatten
    }

  /**
   * Decodes one `@join__field` application. Total: unreadable arguments read as absent.
   */
  private def joinField(directive: Directive): JoinField =
    JoinField(
      graph = graphArgument(directive),
      requires = string(directive, "requires"),
      provides = string(directive, "provides"),
      fieldType = string(directive, "type"),
      external = boolean(directive, "external", default = false),
      overrideFrom = string(directive, "override"),
      overrideLabel = string(directive, "overrideLabel"),
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
        key = string(directive, "key"),
        resolvable = boolean(directive, "resolvable", default = true),
        isInterfaceObject = boolean(directive, "isInterfaceObject", default = false)
      )
    }

  private def joinTypes(directives: List[Directive], names: JoinNames): List[Directive] =
    directives.filter(directive => names.tpe.contains(directive.name))

  /**
   * Graph keys a type belongs to. A type with no `@join__type` at all is a shared value type.
   */
  private def typeGraphs(directives: List[Directive], ctx: ProjectionContext): List[String] =
    joinTypes(directives, ctx.names) match {
      case Nil     => ctx.keys
      case entries => entries.flatMap(joinType).map(_.graph).distinct
    }

  /**
   * Graph keys that resolve `field`, with each graph's metadata when it declared any.
   *
   * A field with no `@join__field` belongs to every member graph. A field whose declarations name
   * no graph is a value-type field, also resolved by every member. Otherwise only the named graphs
   * resolve it.
   */
  private def fieldGraphs(
    field: FieldDefinition,
    members: List[String],
    names: JoinNames
  ): Map[String, Option[JoinField]] = {
    val entries = field.directives.filter(directive => names.field.contains(directive.name)).map(joinField)

    if (entries.isEmpty) members.map(_ -> None).toMap
    else {
      val scoped = entries.flatMap(entry => entry.graph.map(_ -> Some(entry))).toMap
      if (scoped.isEmpty) members.map(_ -> entries.headOption).toMap
      else scoped
    }
  }

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

  private def validate(document: Document, ctx: ProjectionContext): List[String] = {
    val contexts = declaredContexts(document, ctx)

    (extensionDiagnostics(document) :::
      document.typeDefinitions.flatMap(typeDiagnostics(_, ctx)) :::
      document.typeDefinitions.flatMap(fieldDiagnostics(_, ctx, contexts))).distinct.sorted
  }

  /**
   * Every `@context` name the supergraph declares, still namespaced as the composer wrote it.
   */
  private def declaredContexts(document: Document, ctx: ProjectionContext): Set[String] =
    document.typeDefinitions
      .flatMap(_.directives)
      .filter(directive => ctx.contextNames.contains(directive.name))
      .flatMap(string(_, "name"))
      .toSet

  private def extensionDiagnostics(document: Document): List[String] =
    if (document.typeExtensions.isEmpty) Nil
    else List("[supergraph] A supergraph is fully composed and must not declare type extensions.")

  private def typeDiagnostics(definition: TypeDefinition, ctx: ProjectionContext): List[String] = {
    val entries  = joinTypes(definition.directives, ctx.names).map(joinType)
    val missing  =
      if (entries.exists(_.isEmpty))
        List(s"[supergraph] Type '${definition.name}' has a join type entry without a 'graph' argument.")
      else Nil
    val unknown  = entries.flatten
      .map(_.graph)
      .filterNot(ctx.nameByKey.contains)
      .distinct
      .sorted
      .map(graph => s"[supergraph] Type '${definition.name}' names graph '$graph', which the graph enum omits.")
    // The declaring graph must receive the type for its context to survive projection.
    // Unrecognized namespaces are checked only when a field references them, allowing unused
    // declarations from composers with a different naming convention.
    val declared = typeGraphs(definition.directives, ctx)
    val contexts = definition.directives
      .filter(directive => ctx.contextNames.contains(directive.name))
      .flatMap(string(_, "name"))
      .flatMap(name => contextOwner(name, ctx).map(owner => name -> owner._1))
      .collect {
        case (name, graph) if !declared.contains(ctx.keyByName.getOrElse(graph, graph)) =>
          s"[supergraph] Type '${definition.name}' declares context '$name' for graph '$graph', " +
            "which does not declare the type."
      }

    missing ::: unknown ::: contexts
  }

  private def fieldDiagnostics(
    definition: TypeDefinition,
    ctx: ProjectionContext,
    declaredContexts: Set[String]
  ): List[String] = {
    val members          = typeGraphs(definition.directives, ctx)
    val fields           = definition match {
      case value: ObjectTypeDefinition    => value.fields
      case value: InterfaceTypeDefinition => value.fields
      case _                              => Nil
    }
    // Only Federation 2 annotates every type, so an unannotated root is a `join/v0.1` shared value
    // type: left to the composer rather than rejected wholesale here.
    val subscriptionRoot = ctx.subscriptionRoot.contains(definition.name) &&
      joinTypes(definition.directives, ctx.names).nonEmpty

    fields.flatMap { field =>
      val fieldName  = s"${definition.name}.${field.name}"
      val entries    = field.directives.filter(directive => ctx.names.field.contains(directive.name)).map(joinField)
      val scoped     = entries.flatMap(_.graph)
      val unknown    = scoped
        .filterNot(members.contains)
        .distinct
        .sorted
        .map(graph => s"[supergraph] Field '$fieldName' names graph '$graph', which does not declare the type.")
      val repeated   = scoped
        .groupBy(identity)
        .collect { case (graph, _ :: _ :: _) =>
          s"[supergraph] Field '$fieldName' declares more than one entry for graph '$graph'."
        }
        .toList
      val types      = entries
        .flatMap(_.fieldType)
        .flatMap(parseFieldType(_).left.toOption)
        .map(value => s"[supergraph] Field '$fieldName' declares the unparseable type '$value'.")
      // Context arguments exist only in join metadata; invalid types cannot fall back to the field.
      val contextual = entries.flatMap { value =>
        value.contextArguments.toList.flatten.map(value.graph.flatMap(ctx.nameByKey.get) -> _)
      }
      val argTypes   = contextual.flatMap { case (_, argument) => parseFieldType(argument.contextType).left.toOption }
        .map(value => s"[supergraph] Field '$fieldName' declares the unparseable context argument type '$value'.")
      // The selection is the half of the `@fromContext` argument the context name is prepended to,
      // so an empty one projects `@fromContext(field: "$viewer")`, which no subgraph can parse.
      val selections = contextual.collect {
        case (_, argument) if argument.selection.trim.isEmpty =>
          s"[supergraph] Field '$fieldName' declares an empty context argument selection " +
            s"for context '${argument.context}'."
      }
      val contexts   = contextual.flatMap { case (graph, argument) =>
        if (!declaredContexts.contains(argument.context))
          List(s"[supergraph] Field '$fieldName' names the undeclared context '${argument.context}'.")
        else {
          // Federation requires `@context` and `@fromContext` in the same subgraph, so an entry
          // naming another graph's context would project an argument no subgraph can resolve.
          val declaring = contextOwner(argument.context, ctx).map(_._1)
          graph
            .filterNot(declaring.contains)
            .map(name =>
              s"[supergraph] Field '$fieldName' names context '${argument.context}', " +
                s"which graph '$name' does not declare."
            )
            .toList
        }
      }
      val unroutable =
        if (subscriptionRoot && resolvingSubgraphCount(fieldGraphs(field, members, ctx.names), ctx) > 1)
          List(
            s"[supergraph] Subscription field '$fieldName' is resolved by more than one graph, " +
              "which the gateway cannot route."
          )
        else Nil

      unknown ::: repeated ::: types ::: argTypes ::: selections ::: contexts ::: unroutable
    }
  }

  private def project(document: Document, key: String, ctx: ProjectionContext): Document = {
    val definitions = document.definitions.flatMap {
      case definition: TypeDefinition      => projectType(definition, key, ctx).toList
      case definition: DirectiveDefinition => if (ctx.isFeatureDefinition(definition.name)) Nil else List(definition)
      case _                               => Nil
    }

    Document(projectSchema(definitions, document, key, ctx) :: definitions, SourceMapper.empty)
  }

  private def projectType(definition: TypeDefinition, key: String, ctx: ProjectionContext): Option[TypeDefinition] =
    if (ctx.isFeatureName(definition.name)) None
    else {
      val members = typeGraphs(definition.directives, ctx)
      if (!members.contains(key)) None
      else {
        val entries    = joinTypes(definition.directives, ctx.names).flatMap(joinType).filter(_.graph == key)
        val directives = projectDirectives(definition.directives, ctx) :::
          entries.flatMap(keyDirective) :::
          (if (entries.exists(_.isInterfaceObject)) List(Directive("interfaceObject")) else Nil) :::
          contextDeclarations(definition.directives, key, ctx) :::
          composedDirectives(definition.directives, key, ctx)

        val projected: TypeDefinition = definition match {
          case value: ObjectTypeDefinition      =>
            value.copy(
              implements = projectImplements(value.implements, value.directives, key, ctx),
              directives = directives,
              fields = projectFields(value.fields, members, key, ctx)
            )
          case value: InterfaceTypeDefinition   =>
            value.copy(
              implements = projectImplements(value.implements, value.directives, key, ctx),
              directives = directives,
              fields = projectFields(value.fields, members, key, ctx)
            )
          case value: UnionTypeDefinition       =>
            value.copy(
              directives = directives,
              memberTypes = projectUnionMembers(value.memberTypes, value.directives, key, ctx)
            )
          case value: EnumTypeDefinition        =>
            value.copy(directives = directives, enumValuesDefinition = projectEnumValues(value, key, ctx))
          case value: InputObjectTypeDefinition =>
            value.copy(directives = directives, fields = projectInputFields(value.fields, key, ctx))
          case value: ScalarTypeDefinition      =>
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
    case value: ObjectTypeDefinition      => value.fields.isEmpty
    case value: InterfaceTypeDefinition   => value.fields.isEmpty
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
    ctx.nameByKey.get(key).toList.flatMap { graph =>
      directives.filter(directive => ctx.contextNames.contains(directive.name)).flatMap { directive =>
        string(directive, "name").flatMap(contextOwner(_, ctx)).collect { case (`graph`, local) =>
          Directive("context", Map[String, InputValue]("name" -> StringValue(local)))
        }
      }
    }

  /**
   * Restores arguments removed from the supergraph field and recorded in join metadata.
   * Their original positions are unavailable, so append them. SchemaComposer hides these
   * arguments from clients when composing the projected subgraphs.
   */
  private def contextArgumentDefinitions(
    entry: Option[JoinField],
    key: String,
    ctx: ProjectionContext
  ): List[InputValueDefinition] =
    ctx.nameByKey.get(key).toList.flatMap { graph =>
      entry.toList.flatMap(_.contextArguments.toList.flatten).flatMap { argument =>
        contextOwner(argument.context, ctx).collect { case (`graph`, local) =>
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
    }

  /**
   * Re-emits `@join__directive(graphs:, name:, args:)` as the directive it stands for.
   */
  private def composedDirectives(directives: List[Directive], key: String, ctx: ProjectionContext): List[Directive] =
    directives.filter(directive => ctx.names.directive.contains(directive.name)).flatMap { directive =>
      val graphs = directive.arguments
        .get("graphs")
        .collect { case InputValue.ListValue(values) => values.collect { case EnumValue(value) => value } }
        .getOrElse(Nil)
      val args   = directive.arguments
        .get("args")
        .collect { case InputValue.ObjectValue(fields) => fields }
        .getOrElse(Map.empty[String, InputValue])

      if (graphs.contains(key)) string(directive, "name").map(name => Directive(name.stripPrefix("@"), args)).toList
      else Nil
    }

  private def projectFields(
    fields: List[FieldDefinition],
    members: List[String],
    key: String,
    ctx: ProjectionContext
  ): List[FieldDefinition] =
    fields.flatMap { field =>
      val owners = fieldGraphs(field, members, ctx.names)
      owners.get(key).map { entry =>
        projectField(field, entry, key, ctx, shareable = resolves(entry) && resolvingSubgraphCount(owners, ctx) > 1)
      }
    }

  /**
   * True when a graph actually resolves the field, rather than merely declaring it.
   */
  private def resolves(entry: Option[JoinField]): Boolean =
    !entry.exists(value => value.external || value.usedOverridden)

  /**
   * Graphs that actually resolve the field: declared owners, minus the ones that only declare it,
   * minus any graph another graph has overridden away.
   */
  private def resolvingSubgraphCount(owners: Map[String, Option[JoinField]], ctx: ProjectionContext): Int = {
    val overridden = owners.valuesIterator.flatten.flatMap(_.overrideFrom).flatMap(ctx.keyByName.get).toSet
    owners.count { case (graph, entry) => resolves(entry) && !overridden.contains(graph) }
  }

  private def projectField(
    field: FieldDefinition,
    entry: Option[JoinField],
    key: String,
    ctx: ProjectionContext,
    shareable: Boolean
  ): FieldDefinition = {
    val translated = entry.toList.flatMap { value =>
      value.requires.map(fields => Directive("requires", Map("fields" -> StringValue(fields)))).toList :::
        value.provides.map(fields => Directive("provides", Map("fields" -> StringValue(fields)))).toList :::
        (if (value.external || (value.usedOverridden && value.overrideLabel.isEmpty)) List(Directive("external"))
         else Nil) :::
        value.overrideFrom
          .map(from =>
            Directive(
              "override",
              List(
                Some("from" -> StringValue(from)),
                value.overrideLabel.map(label => "label" -> StringValue(label))
              ).flatten.toMap
            )
          )
          .toList
    } :::
      // Outside the entry: a field with no join entry at all is the default-ownership case, which
      // lands in every member graph and so is the one that most needs declaring shareable.
      (if (shareable) List(Directive("shareable")) else Nil)
    // `validate` already proved every declared type parses, so the fallback is unreachable.
    val ofType     = entry
      .flatMap(_.fieldType)
      .flatMap(parseFieldType(_).toOption)
      .getOrElse(field.ofType)

    field.copy(
      ofType = ofType,
      directives = projectDirectives(field.directives, ctx) ::: translated ::: composedDirectives(
        field.directives,
        key,
        ctx
      ),
      args = field.args.map(argument => argument.copy(directives = projectDirectives(argument.directives, ctx))) :::
        contextArgumentDefinitions(entry, key, ctx)
    )
  }

  /**
   * Per-graph filters share one rule: when the directive appears nowhere on the element, every
   * graph keeps the full list. Older join versions omit these directives entirely, and "absent"
   * must mean "shared by all" rather than "owned by none".
   */
  private def projectImplements(
    implements: List[NamedType],
    directives: List[Directive],
    key: String,
    ctx: ProjectionContext
  ): List[NamedType] = {
    val entries = directives.filter(directive => ctx.names.implements.contains(directive.name))
    if (entries.isEmpty) implements
    else {
      val allowed = entries.filter(graphArgument(_).contains(key)).flatMap(string(_, "interface")).toSet
      implements.filter(value => allowed.contains(value.name))
    }
  }

  private def projectUnionMembers(
    memberTypes: List[String],
    directives: List[Directive],
    key: String,
    ctx: ProjectionContext
  ): List[String] = {
    val entries = directives.filter(directive => ctx.names.unionMember.contains(directive.name))
    if (entries.isEmpty) memberTypes
    else {
      val allowed = entries.filter(graphArgument(_).contains(key)).flatMap(string(_, "member")).toSet
      memberTypes.filter(allowed.contains)
    }
  }

  private def projectEnumValues(
    definition: EnumTypeDefinition,
    key: String,
    ctx: ProjectionContext
  ): List[EnumValueDefinition] = {
    def marks(value: EnumValueDefinition): List[Directive] =
      value.directives.filter(directive => ctx.names.enumValue.contains(directive.name))

    val retained =
      if (definition.enumValuesDefinition.forall(marks(_).isEmpty)) definition.enumValuesDefinition
      else definition.enumValuesDefinition.filter(marks(_).exists(graphArgument(_).contains(key)))

    retained.map(value => value.copy(directives = projectDirectives(value.directives, ctx)))
  }

  private def projectInputFields(
    fields: List[InputValueDefinition],
    key: String,
    ctx: ProjectionContext
  ): List[InputValueDefinition] =
    fields.flatMap { field =>
      val graphs   = field.directives.iterator
        .filter(directive => ctx.names.field.contains(directive.name))
        .flatMap(graphArgument)
        .toSet
      val included = graphs.isEmpty || graphs.contains(key)

      if (included) Some(field.copy(directives = projectDirectives(field.directives, ctx))) else None
    }

  /**
   * Names an operation root only when its type survived projection with at least one field.
   */
  private def projectSchema(
    definitions: List[Definition],
    document: Document,
    key: String,
    ctx: ProjectionContext
  ): SchemaDefinition = {
    val populated = definitions.collect {
      case value: ObjectTypeDefinition if value.fields.nonEmpty => value.name
    }.toSet
    val declared  = document.schemaDefinition

    def root(name: Option[String], fallback: String): Option[String] =
      name.orElse(Some(fallback)).filter(populated.contains)

    val query        = root(declared.flatMap(_.query), "Query")
    val mutation     = root(declared.flatMap(_.mutation), "Mutation")
    val subscription = root(declared.flatMap(_.subscription), "Subscription")
    val directives   = composedDirectives(DirectiveComposition.schemaDirectives(document), key, ctx)

    // Even a graph without roots needs the federation link so SchemaComposer recognizes its
    // entity keys and routing directives.
    SchemaDefinition(FederationLink :: directives, query, mutation, subscription, None)
  }

  private final case class ProjectionContext(
    feature: LinkedFeature,
    names: JoinNames,
    registry: List[Graph],
    prefixes: Set[String],
    linkNames: Set[String],
    claimedDefinitions: Set[String],
    subscriptionRoot: Option[String],
    contextNames: Set[String],
    projectedFeatures: List[LinkedFeature]
  ) {
    val keys: List[String]             = registry.map(_.key)
    val keyByName: Map[String, String] = registry.map(g => g.name -> g.key).toMap
    val nameByKey: Map[String, String] = registry.map(g => g.key -> g.name).toMap
    val graphNames: Set[String]        = keyByName.keySet

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
      projectedFeatures.iterator.map { feature =>
        feature.sourceDirective(name).filter(ProjectedFeatures(feature.identity).contains)
      }.collectFirst { case Some(name) => name }
  }

  private final case class JoinType(
    graph: String,
    key: Option[String],
    resolvable: Boolean,
    isInterfaceObject: Boolean
  )

  private final case class JoinField(
    graph: Option[String],
    requires: Option[String],
    provides: Option[String],
    fieldType: Option[String],
    external: Boolean,
    overrideFrom: Option[String],
    overrideLabel: Option[String],
    contextArguments: Option[List[ContextArgument]],
    usedOverridden: Boolean
  )

  private final case class JoinNames(
    tpe: Set[String],
    field: Set[String],
    implements: Set[String],
    unionMember: Set[String],
    enumValue: Set[String],
    directive: Set[String]
  )

  private final case class ContextArgument(
    name: String,
    contextType: String,
    context: String,
    selection: String
  )

  private val JoinIdentity      = "https://specs.apollo.dev/join"
  private val LinkIdentity      = "https://specs.apollo.dev/link"
  private val ContextIdentity   = "https://specs.apollo.dev/context"
  private val FederationImports = List(
    "@key",
    "@shareable",
    "@external",
    "@requires",
    "@provides",
    "@tag",
    "@override",
    "@inaccessible",
    "@interfaceObject",
    "@authenticated",
    "@requiresScopes",
    "@policy",
    "@context",
    "@fromContext",
    "@cost",
    "@listSize"
  )

  private val ProjectedFeatures = Map(
    DirectiveComposition.FederationIdentity   -> FederationImports
      .map(_.stripPrefix("@"))
      .toSet
      .diff(Set("context", "fromContext")),
    "https://specs.apollo.dev/inaccessible"   -> Set("inaccessible"),
    "https://specs.apollo.dev/tag"            -> Set("tag"),
    "https://specs.apollo.dev/authenticated"  -> Set("authenticated"),
    "https://specs.apollo.dev/requiresScopes" -> Set("requiresScopes"),
    "https://specs.apollo.dev/policy"         -> Set("policy"),
    "https://specs.apollo.dev/cost"           -> Set("cost", "listSize")
  )
  private val FederationLink    = Directive(
    "link",
    Map[String, InputValue](
      "url"    -> StringValue("https://specs.apollo.dev/federation/v2.9"),
      "import" -> InputValue.ListValue(FederationImports.map(StringValue(_)))
    )
  )
}
