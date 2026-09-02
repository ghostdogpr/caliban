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
import sttp.model.Uri

/**
 * Decomposes an Apollo Federation supergraph into the subgraph documents it was composed from.
 *
 * Every failure is collected by [[validate]] before a single definition is projected, which keeps
 * the projection functions total: only [[context]] and [[decompose]] carry an error channel.
 */
private[gateway] object SupergraphDecomposition {
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
    "@fromContext"
  )
  private val FederationLink    = Directive(
    "link",
    Map[String, InputValue](
      "url"    -> StringValue("https://specs.apollo.dev/federation/v2.9"),
      "import" -> InputValue.ListValue(FederationImports.map(StringValue(_)))
    )
  )

  /** One entry of the join graph enum: a subgraph's identity and endpoint. */
  final case class Graph(key: String, name: String, url: Uri)

  /** A subgraph projected out of the supergraph. */
  final case class Projected(graph: Graph, document: Document)

  def decompose(document: Document): Either[List[String], List[Projected]] =
    for {
      ctx        <- context(document)
      diagnostics = validate(document, ctx)
      projected  <- if (diagnostics.nonEmpty) Left(diagnostics)
                    else Right(ctx.registry.map(graph => Projected(graph, project(document, graph.key, ctx))))
    } yield projected

  def graphs(document: Document): Either[List[String], List[Graph]] =
    for {
      feature    <- joinFeature(document).left.map(List(_))
      enumType   <- graphEnum(document, feature).left.map(List(_))
      names       = feature.directiveNames("graph")
      results     = enumType.enumValuesDefinition.map(graphEntry(names, _))
      failures    = results.collect { case Left(errors) => errors }.flatten
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

  private def joinFeature(document: Document): Either[String, LinkedFeature] =
    DirectiveComposition
      .linkedFeatures(document)
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

        // `Uri.parse` is extremely lenient: "foo", "not a uri" and "://nohost" all parse into
        // relative or hostless URIs. Parsing alone is not validation, so require an absolute
        // http(s) endpoint rather than let an unusable URL fail later at request time.
        val url = string(directive, "url") match {
          case None        => Left(List(s"$prefix must declare a 'url' argument."))
          case Some(value) =>
            Uri
              .parse(value)
              .toOption
              .filter(isHttpEndpoint)
              .toRight(List(s"$prefix must declare an absolute http or https 'url'."))
        }

        (name, url) match {
          case (Right(name), Right(url)) => Right(Graph(value.enumValue, name, url))
          case (name, url)               => Left(name.left.getOrElse(Nil) ::: url.left.getOrElse(Nil))
        }
    }

  private def isHttpEndpoint(uri: Uri): Boolean =
    uri.scheme.map(_.toLowerCase(java.util.Locale.ROOT)).exists(s => s == "http" || s == "https") &&
      uri.host.exists(_.nonEmpty)

  // ---------------------------------------------------------------------------------------------
  // Context
  // ---------------------------------------------------------------------------------------------

  private def context(document: Document): Either[List[String], Context] =
    for {
      feature  <- joinFeature(document).left.map(List(_))
      registry <- graphs(document)
    } yield {
      val features         = DirectiveComposition.linkedFeatures(document)
      val prefixes         = features.iterator.map(_.namespace).toSet.+("link").map(_ + "__")
      val linkNames        = features.filter(_.identity == LinkIdentity).flatMap(_.directiveNames("link")).toSet + "link"
      val claimed          = document.directiveDefinitions.iterator
        .map(_.name)
        .filter(name => features.exists(_.sourceDirective(name).isDefined))
        .toSet
      val subscriptionRoot = document.schemaDefinition.flatMap(_.subscription)
      // `@context` is applied by the supergraph itself rather than by the join feature, so its
      // names come from the context feature and are empty for a supergraph that declares none.
      val contexts         = features.filter(_.identity == ContextIdentity).flatMap(_.directiveNames("context")).toSet

      Context(feature, joinNames(feature), registry, prefixes, linkNames, claimed, subscriptionRoot, contexts)
    }

  private def joinNames(feature: LinkedFeature): JoinNames =
    JoinNames(
      graph = feature.directiveNames("graph"),
      tpe = feature.directiveNames("type"),
      field = feature.directiveNames("field"),
      implements = feature.directiveNames("implements"),
      unionMember = feature.directiveNames("unionMember"),
      enumValue = feature.directiveNames("enumValue"),
      directive = feature.directiveNames("directive")
    )

  private final case class Context(
    feature: LinkedFeature,
    names: JoinNames,
    registry: List[Graph],
    prefixes: Set[String],
    linkNames: Set[String],
    claimedDefinitions: Set[String],
    subscriptionRoot: Option[String],
    contextNames: Set[String]
  ) {
    val keys: List[String]             = registry.map(_.key)
    val keyByName: Map[String, String] = registry.map(g => g.name -> g.key).toMap
    val nameByKey: Map[String, String] = registry.map(g => g.key -> g.name).toMap
    val graphNames: Set[String]        = registry.map(_.name).toSet

    /** A type or directive name owned by a linked feature, so absent from subgraph output. */
    def stripped(name: String): Boolean = prefixes.exists(name.startsWith)

    /**
     * Directive applications removed from subgraph output: join and link machinery, plus
     * `@context`. `@inaccessible`, `@tag`, `@deprecated` and composed custom directives all
     * survive as written.
     *
     * `@context` is the exception because it is the one federation directive a supergraph applies
     * with a subgraph-namespaced argument: `@context(name: "orders__userContext")` names the graph
     * that declared it, so it cannot be carried over unchanged. It is dropped here and re-emitted,
     * for the one graph that owns it, by [[contextDeclarations]].
     */
    def stripApplication(name: String): Boolean =
      feature.sourceDirective(name).isDefined || linkNames.contains(name) || stripped(name) ||
        contextNames.contains(name)

    /** Directive definitions removed from subgraph output: anything a linked feature supplies. */
    def stripDefinition(name: String): Boolean = claimedDefinitions.contains(name) || stripped(name)
  }

  // ---------------------------------------------------------------------------------------------
  // Join metadata decoders
  // ---------------------------------------------------------------------------------------------

  private def string(directive: Directive, name: String): Option[String] =
    directive.arguments.get(name).collect { case StringValue(value) => value }

  private def boolean(directive: Directive, name: String, default: Boolean): Boolean =
    directive.arguments.get(name).collect { case BooleanValue(value) => value }.getOrElse(default)

  private def graphArgument(directive: Directive): Option[String] =
    directive.arguments.get("graph").collect { case EnumValue(value) => value }

  /**
   * Splits a supergraph context name into the subgraph that declared it and the name that subgraph
   * wrote. Both rover and Hive namespace as `<subgraph name>__<name>`, using the graph *name* and
   * not the graph enum key, and with no sanitizing at all: a subgraph named `other graph` writes
   * `other graph__userContext` against an enum key of `OTHER_GRAPH`. A subgraph name may itself
   * contain the separator while a context name may not — the composer requires `[A-Za-z][A-Za-z0-9]*`
   * — so the last separator is the split.
   */
  private def contextOwner(name: String, ctx: Context): Option[(String, String)] =
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

  /** Decodes one `@join__field` application. Total: unreadable arguments read as absent. */
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

  /** Decodes one `@join__type` application. `graph:` is non-null in the spec, so this can fail. */
  private def joinType(directive: Directive): Option[JoinType] =
    graphArgument(directive).map { graph =>
      JoinType(
        graph = graph,
        key = string(directive, "key"),
        extension = boolean(directive, "extension", default = false),
        resolvable = boolean(directive, "resolvable", default = true),
        isInterfaceObject = boolean(directive, "isInterfaceObject", default = false)
      )
    }

  private def joinTypes(directives: List[Directive], names: JoinNames): List[Directive] =
    directives.filter(directive => names.tpe.contains(directive.name))

  /** Graph keys a type belongs to. A type with no `@join__type` at all is a shared value type. */
  private def typeMembers(directives: List[Directive], ctx: Context): List[String] =
    joinTypes(directives, ctx.names) match {
      case Nil     => ctx.keys
      case entries => entries.flatMap(joinType).map(_.graph).distinct
    }

  /** Graph keys that resolve `field`, with each graph's metadata when it declared any. */
  private def fieldGraphs(
    field: FieldDefinition,
    members: List[String],
    names: JoinNames
  ): Map[String, Option[JoinField]] = {
    val entries = field.directives.filter(directive => names.field.contains(directive.name)).map(joinField)
    val scoped  = entries.filter(_.graph.nonEmpty)

    if (entries.isEmpty) members.map(_ -> None).toMap // (1) belongs to every member
    else if (scoped.isEmpty) members.map(_ -> entries.headOption).toMap // (2) value-type field
    else scoped.map(entry => entry.graph.get -> Some(entry)).toMap // (3) exactly these graphs
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

  // ---------------------------------------------------------------------------------------------
  // Validation
  // ---------------------------------------------------------------------------------------------

  private def validate(document: Document, ctx: Context): List[String] = {
    val contexts = declaredContexts(document, ctx)

    (extensionDiagnostics(document) :::
      document.typeDefinitions.flatMap(typeDiagnostics(_, ctx)) :::
      document.typeDefinitions.flatMap(fieldDiagnostics(_, ctx, contexts))).distinct.sorted
  }

  /** Every `@context` name the supergraph declares, still namespaced as the composer wrote it. */
  private def declaredContexts(document: Document, ctx: Context): Set[String] =
    document.typeDefinitions
      .flatMap(_.directives)
      .filter(directive => ctx.contextNames.contains(directive.name))
      .flatMap(string(_, "name"))
      .toSet

  private def extensionDiagnostics(document: Document): List[String] =
    if (document.typeExtensions.isEmpty) Nil
    else List("[supergraph] A supergraph is fully composed and must not declare type extensions.")

  private def typeDiagnostics(definition: TypeDefinition, ctx: Context): List[String] = {
    val entries  = joinTypes(definition.directives, ctx.names)
    val missing  =
      if (entries.exists(joinType(_).isEmpty))
        List(s"[supergraph] Type '${definition.name}' has a join type entry without a 'graph' argument.")
      else Nil
    val unknown  = entries
      .flatMap(joinType)
      .map(_.graph)
      .filterNot(ctx.keys.contains)
      .distinct
      .sorted
      .map(graph => s"[supergraph] Type '${definition.name}' names graph '$graph', which the graph enum omits.")
    // A context is declared on the type itself and namespaced by the graph that wrote it, so a
    // declaration naming a graph the type does not belong to can be projected nowhere: that graph
    // never receives the type. Unchecked it surfaces much later, as a composition error against a
    // projection, for a field entry that names the context and looks perfectly consistent here.
    //
    // A name that resolves to no subgraph at all is deliberately not reported: a supergraph that
    // namespaces differently would be rejected wholesale over a declaration nothing references,
    // and one that is referenced is caught from the field side instead.
    val declared = typeMembers(definition.directives, ctx)
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
    ctx: Context,
    declaredContexts: Set[String]
  ): List[String] = {
    val members          = typeMembers(definition.directives, ctx)
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
      val coordinate = s"${definition.name}.${field.name}"
      val entries    = field.directives.filter(directive => ctx.names.field.contains(directive.name))
      val scoped     = entries.flatMap(joinField(_).graph)
      val unknown    = scoped
        .filterNot(members.contains)
        .distinct
        .sorted
        .map(graph => s"[supergraph] Field '$coordinate' names graph '$graph', which does not declare the type.")
      val repeated   = scoped
        .groupBy(identity)
        .collect { case (graph, _ :: _ :: _) =>
          s"[supergraph] Field '$coordinate' declares more than one entry for graph '$graph'."
        }
        .toList
      val types      = entries
        .flatMap(joinField(_).fieldType)
        .flatMap(parseFieldType(_).left.toOption)
        .map(value => s"[supergraph] Field '$coordinate' declares the unparseable type '$value'.")
      // A context argument exists only inside the join metadata — the composer strips it from the
      // field it belongs to — so an unreadable entry loses the argument silently rather than
      // producing a wrong one, which is why it is a diagnostic and not a projection fallback.
      val contextual = entries.map(joinField).flatMap { value =>
        value.contextArguments.toList.flatten.map(value.graph.flatMap(ctx.nameByKey.get) -> _)
      }
      val argTypes   = contextual.flatMap { case (_, argument) => parseFieldType(argument.contextType).left.toOption }
        .map(value => s"[supergraph] Field '$coordinate' declares the unparseable context argument type '$value'.")
      // The selection is the half of the `@fromContext` argument the context name is prepended to,
      // so an empty one projects `@fromContext(field: "$viewer")`, which no subgraph can parse.
      val selections = contextual.collect {
        case (_, argument) if argument.selection.trim.isEmpty =>
          s"[supergraph] Field '$coordinate' declares an empty context argument selection " +
            s"for context '${argument.context}'."
      }
      val contexts   = contextual.flatMap { case (graph, argument) =>
        if (!declaredContexts.contains(argument.context))
          List(s"[supergraph] Field '$coordinate' names the undeclared context '${argument.context}'.")
        else {
          // Federation requires `@context` and `@fromContext` in the same subgraph, so an entry
          // naming another graph's context would project an argument no subgraph can resolve.
          val declaring = contextOwner(argument.context, ctx).map(_._1)
          graph
            .filterNot(declaring.contains)
            .map(name =>
              s"[supergraph] Field '$coordinate' names context '${argument.context}', " +
                s"which graph '$name' does not declare."
            )
            .toList
        }
      }
      val unroutable =
        if (subscriptionRoot && providerCount(fieldGraphs(field, members, ctx.names), ctx) > 1)
          List(
            s"[supergraph] Subscription field '$coordinate' is resolved by more than one graph, " +
              "which the gateway cannot route."
          )
        else Nil

      unknown ::: repeated ::: types ::: argTypes ::: selections ::: contexts ::: unroutable
    }
  }

  // ---------------------------------------------------------------------------------------------
  // Projection — total; validation has already run
  // ---------------------------------------------------------------------------------------------

  private def project(document: Document, key: String, ctx: Context): Document = {
    val definitions = document.definitions.flatMap {
      case definition: TypeDefinition      => projectType(definition, key, ctx).toList
      case definition: DirectiveDefinition => if (ctx.stripDefinition(definition.name)) Nil else List(definition)
      case _                               => Nil
    }

    Document(schema(definitions, document, key, ctx).toList ::: definitions, SourceMapper.empty)
  }

  private def projectType(definition: TypeDefinition, key: String, ctx: Context): Option[TypeDefinition] =
    if (ctx.stripped(definition.name)) None
    else {
      val members = typeMembers(definition.directives, ctx)
      if (!members.contains(key)) None
      else {
        val mine       = joinTypes(definition.directives, ctx.names).flatMap(joinType).filter(_.graph == key)
        val directives = keptDirectives(definition.directives, ctx) :::
          mine.flatMap(keyDirective) :::
          (if (mine.exists(_.isInterfaceObject)) List(Directive("interfaceObject")) else Nil) :::
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
   * GraphQL forbids an object, interface, union, enum or input object that declares no members.
   * A type the graph belongs to but contributes nothing to projects empty — the ordinary case being
   * a graph that owns no root fields for one operation — so it is dropped rather than rendered as
   * invalid SDL.
   *
   * Dropping can leave a dangling reference, but only for a supergraph that was already malformed:
   * a graph naming a type in a field it can resolve nothing of. Such input produced invalid output
   * before this filter too, as an empty type rather than a missing one.
   */
  private def isEmpty(definition: TypeDefinition): Boolean = definition match {
    case value: ObjectTypeDefinition      => value.fields.isEmpty
    case value: InterfaceTypeDefinition   => value.fields.isEmpty
    case value: UnionTypeDefinition       => value.memberTypes.isEmpty
    case value: EnumTypeDefinition        => value.enumValuesDefinition.isEmpty
    case value: InputObjectTypeDefinition => value.fields.isEmpty
    case _: ScalarTypeDefinition          => false
  }

  private def keptDirectives(directives: List[Directive], ctx: Context): List[Directive] =
    directives.filterNot(directive => ctx.stripApplication(directive.name))

  private def keyDirective(entry: JoinType): Option[Directive] =
    entry.key.map { fields =>
      val arguments = Map[String, InputValue]("fields" -> StringValue(fields)) ++
        (if (entry.resolvable) Map.empty[String, InputValue] else Map("resolvable" -> BooleanValue(false)))
      Directive("key", arguments)
    }

  /**
   * Re-emits the `@context` declarations this graph owns under the name it wrote, dropping the
   * ones another graph declared. The type carries every graph's declaration side by side, so the
   * namespace is the only thing that says which graph a declaration came from.
   */
  private def contextDeclarations(directives: List[Directive], key: String, ctx: Context): List[Directive] =
    ctx.nameByKey.get(key).toList.flatMap { graph =>
      directives.filter(directive => ctx.contextNames.contains(directive.name)).flatMap { directive =>
        string(directive, "name").flatMap(contextOwner(_, ctx)).collect { case (`graph`, local) =>
          Directive("context", Map[String, InputValue]("name" -> StringValue(local)))
        }
      }
    }

  /**
   * Rebuilds the arguments the composer folded into `@join__field(contextArguments:)`. A context
   * argument is removed from the supergraph field outright — `amount(currency: String)` composes
   * to `amount: Int!` — so the argument, its type and its selection survive only in that
   * metadata, and only the declaring graph can be given the argument back.
   *
   * The rebuilt arguments are appended rather than restored to their authored positions, which the
   * supergraph does not record. Nothing downstream can see the difference: `SchemaComposer` hides
   * every context argument from the composed field.
   */
  private def contextArgumentDefinitions(
    entry: Option[JoinField],
    key: String,
    ctx: Context
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

  /** Re-emits `@join__directive(graphs:, name:, args:)` as the directive it stands for. */
  private def composedDirectives(directives: List[Directive], key: String, ctx: Context): List[Directive] =
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
    ctx: Context
  ): List[FieldDefinition] =
    fields.flatMap { field =>
      val owners = fieldGraphs(field, members, ctx.names)
      owners.get(key).map { entry =>
        projectField(field, entry, key, ctx, shareable = resolves(entry) && providerCount(owners, ctx) > 1)
      }
    }

  /** True when a graph actually resolves the field, rather than merely declaring it. */
  private def resolves(entry: Option[JoinField]): Boolean =
    !entry.exists(value => value.external || value.usedOverridden)

  /**
   * Graphs that actually resolve the field: declared owners, minus the ones that only declare it,
   * minus any graph another graph has overridden away.
   */
  private def providerCount(owners: Map[String, Option[JoinField]], ctx: Context): Int = {
    val overridden = owners.values.flatten.flatMap(_.overrideFrom).flatMap(ctx.keyByName.get).toSet
    owners.count { case (graph, entry) => resolves(entry) && !overridden.contains(graph) }
  }

  private def projectField(
    field: FieldDefinition,
    entry: Option[JoinField],
    key: String,
    ctx: Context,
    shareable: Boolean
  ): FieldDefinition = {
    val translated = entry.toList.flatMap { value =>
      value.requires.map(fields => Directive("requires", Map("fields" -> StringValue(fields)))).toList :::
        value.provides.map(fields => Directive("provides", Map("fields" -> StringValue(fields)))).toList :::
        (if (value.external || value.usedOverridden) List(Directive("external")) else Nil) :::
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
      directives = keptDirectives(field.directives, ctx) ::: translated ::: composedDirectives(
        field.directives,
        key,
        ctx
      ),
      args = field.args.map(argument => argument.copy(directives = keptDirectives(argument.directives, ctx))) :::
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
    ctx: Context
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
    ctx: Context
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
    ctx: Context
  ): List[EnumValueDefinition] = {
    def marks(value: EnumValueDefinition): List[Directive] =
      value.directives.filter(directive => ctx.names.enumValue.contains(directive.name))

    val retained =
      if (definition.enumValuesDefinition.forall(marks(_).isEmpty)) definition.enumValuesDefinition
      else definition.enumValuesDefinition.filter(marks(_).exists(graphArgument(_).contains(key)))

    retained.map(value => value.copy(directives = keptDirectives(value.directives, ctx)))
  }

  private def projectInputFields(
    fields: List[InputValueDefinition],
    key: String,
    ctx: Context
  ): List[InputValueDefinition] =
    fields.flatMap { field =>
      val entries  = field.directives.filter(directive => ctx.names.field.contains(directive.name)).map(joinField)
      val scoped   = entries.filter(_.graph.nonEmpty)
      val included = entries.isEmpty || scoped.isEmpty || scoped.exists(_.graph.contains(key))

      if (included) Some(field.copy(directives = keptDirectives(field.directives, ctx))) else None
    }

  /** Names an operation root only when its type survived projection with at least one field. */
  private def schema(
    definitions: List[Definition],
    document: Document,
    key: String,
    ctx: Context
  ): Option[SchemaDefinition] = {
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

    // Always emitted, even for a graph with no roots at all: `SchemaComposer` derives every
    // federation directive name set from the linked features, so a projection without this link
    // composes as a non-federation graph -- empty `key`/`external`/`shareable` sets, no entity
    // lookups, and silently wrong routing rather than a diagnostic.
    Some(SchemaDefinition(FederationLink :: directives, query, mutation, subscription, None))
  }

  // ---------------------------------------------------------------------------------------------

  private final case class JoinType(
    graph: String,
    key: Option[String],
    extension: Boolean,
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
    graph: Set[String],
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
}
