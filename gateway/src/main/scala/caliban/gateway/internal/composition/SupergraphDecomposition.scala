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

import scala.collection.compat._

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
                    else Right(ctx.registry.map(new SupergraphDecomposition(_, ctx).projected))
    } yield projected).left.map(_.map(message => s"[supergraph] $message").distinct.sorted)

  private def graphs(document: Document, feature: LinkedFeature): Either[List[String], List[Graph]] =
    for {
      enumType         <- graphEnum(document, feature).left.map(List(_))
      names             = feature.directiveNames("graph")
      (errors, entries) = enumType.enumValuesDefinition.map(graphEntry(names, _)).partitionMap(identity)
      empty             = check(
                            enumType.enumValuesDefinition.nonEmpty,
                            s"The join graph enum '${enumType.name}' declares no subgraphs."
                          )
      repeated          =
        duplicates(entries.map(_.name)).map(name => s"Subgraph name '$name' is declared more than once.")
      graphs           <- validated(errors.flatten ::: empty ::: repeated, Right(entries))
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

  private def parseFieldType(value: String): Option[Type] =
    Parser
      .parseQuery(s"type CalibanGatewayJoinProbe { field: $value }")
      .toOption
      .flatMap(_.objectTypeDefinitions.flatMap(_.fields).headOption)
      .map(_.ofType)

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
      .map(graph => s"Type '${definition.name}' names graph '$graph', which the graph enum omits.")
    // The declaring graph must receive the type for its context to survive projection.
    // Unrecognized namespaces are checked only when a field references them, allowing unused
    // declarations from composers with a different naming convention.
    val contexts = declaredContextNames(definition.directives, ctx).flatMap { name =>
      contextOwner(name, ctx).collect {
        case (graph, _) if !ctx.members(definition.name)(graph.key) =>
          s"Type '${definition.name}' declares context '$name' for graph '${graph.name}', " +
            "which does not declare the type."
      }
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
      val fieldName = s"${definition.name}.${field.name}"
      val entries   = ctx.join("field", field.directives).map(joinField)
      val scoped    = entries.flatMap(_.graph)
      val unknown   = scoped
        .filterNot(members.contains)
        .map(graph => s"Field '$fieldName' names graph '$graph', which does not declare the type.")
      val repeated  =
        duplicates(scoped).map(graph => s"Field '$fieldName' declares more than one entry for graph '$graph'.")
      val types     = entries
        .flatMap(_.fieldType)
        .filter(parseFieldType(_).isEmpty)
        .map(value => s"Field '$fieldName' declares the unparseable type '$value'.")
      // Context arguments exist only in join metadata; invalid types cannot fall back to the field.
      val contexts  = entries.flatMap { entry =>
        entry.contextArguments.flatMap { argument =>
          val argType   = Some(argument.contextType)
            .filter(parseFieldType(_).isEmpty)
            .map(value => s"Field '$fieldName' declares the unparseable context argument type '$value'.")
          // The selection is the half of the `@fromContext` argument the context name is prepended to,
          // so an empty one projects `@fromContext(field: "$viewer")`, which no subgraph can parse.
          val selection = check(
            argument.selection.trim.nonEmpty,
            s"Field '$fieldName' declares an empty context argument selection for context '${argument.context}'."
          )
          val owner     =
            if (!ctx.declaredContexts.contains(argument.context))
              List(s"Field '$fieldName' names the undeclared context '${argument.context}'.")
            else {
              // Federation requires `@context` and `@fromContext` in the same subgraph, so an entry
              // naming another graph's context would project an argument no subgraph can resolve.
              val declaring = contextOwner(argument.context, ctx).map { case (owner, _) => owner.key }
              entry.graph
                .filterNot(declaring.contains)
                .flatMap(ctx.nameByKey.get)
                .map(name =>
                  s"Field '$fieldName' names context '${argument.context}', which graph '$name' does not declare."
                )
                .toList
            }
          argType.toList ::: selection ::: owner
        }
      }

      unknown ::: repeated ::: types ::: contexts
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

  private def keyDirective(entry: JoinType): Option[Directive] =
    entry.key.map { fields =>
      val arguments = Map[String, InputValue]("fields" -> StringValue(fields)) ++
        (if (entry.resolvable) Map.empty[String, InputValue] else Map("resolvable" -> BooleanValue(false)))
      Directive("key", arguments)
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
    val subscriptionRoot: String        = document.schemaDefinition.flatMap(_.subscription).getOrElse("Subscription")

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

    /**
     * Graph keys resolving each field in the graphs whose `@join__type` entry projects the type as an object type.
     * Composition rejects `@shareable` on subscription root fields, which a progressive override would otherwise get.
     */
    private lazy val fieldGraphs: Map[(String, String), Set[String]] =
      document.typeDefinitions.collect { case value: AggregationTypeDefinition => value }.flatMap { value =>
        val types        = join("type", value.directives).flatMap(joinType)
        val objectGraphs = (value match {
          case _: ObjectTypeDefinition if value.name != subscriptionRoot => types
          case _                                                         => types.filter(_.isInterfaceObject)
        }).map(_.graph).toSet
        value.fields.map { field =>
          def resolves(graph: String) =
            entry("field", field.directives, graph).map(joinField).exists(e => !e.external && !e.usedOverridden)
          (value.name, field.name) -> objectGraphs.filter(resolves)
        }
      }.toMap

    private lazy val related: Map[String, List[String]] =
      document.objectTypeDefinitions
        .flatMap(value => value.implements.flatMap(i => List(value.name -> i.name, i.name -> value.name)))
        .groupMap(_._1)(_._2)

    /**
     * Apollo's rule: a field is shareable where it resolves when another graph resolves it too. An interface object
     * field and the same field on the implementations it stands in for count as one field.
     */
    def shareable(typeName: String, field: String, key: String): Boolean = {
      def graphs(name: String) = fieldGraphs.getOrElse(name -> field, Set.empty[String])
      graphs(typeName)(key) && (typeName :: related.getOrElse(typeName, Nil)).exists(graphs(_).exists(_ != key))
    }

    def entry(member: String, directives: List[Directive], key: String): Option[Directive] = {
      val entries = join(member, directives)
      if (entries.isEmpty) Some(Directive(member)) else entries.find(graphArgument(_).contains(key))
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

  private final case class JoinContextArgument(name: String, contextType: String, context: String, selection: String)

  private val ProjectedFeatures = Map(
    FederationIdentity   -> FederationDirective.imported.map(_.name).toSet.diff(Set("context", "fromContext")),
    InaccessibleIdentity -> Set("inaccessible"),
    TagIdentity          -> Set("tag")
  ) ++ FederationDirective.imported.groupBy(_.specIdentity).collect { case (Some(identity), members) =>
    identity -> members.map(_.name).toSet
  }
  private val FederationLink    = Directive(
    "link",
    Map[String, InputValue](
      "url"    -> StringValue(s"$FederationIdentity/v2.9"),
      "import" -> InputValue.ListValue(FederationDirective.imported.map(member => StringValue(s"@${member.name}")))
    )
  )
}

import SupergraphDecomposition.{ Graph, ProjectionContext }

private final class SupergraphDecomposition private (graph: Graph, ctx: ProjectionContext) {
  import SupergraphDecomposition._

  def projected: Projected = {
    val definitions = ctx.document.definitions.flatMap {
      case definition: TypeDefinition      => projectType(definition).toList
      case definition: DirectiveDefinition => if (ctx.isFeatureDefinition(definition.name)) Nil else List(definition)
      case _                               => Nil
    }

    Projected(graph, Document(projectSchema(definitions) :: definitions, SourceMapper.empty))
  }

  private def projectType(definition: TypeDefinition): Option[TypeDefinition] =
    if (ctx.isFeatureName(definition.name)) None
    else {
      val entries = ctx.join("type", definition.directives).flatMap(joinType).filter(_.graph == graph.key)
      if (entries.isEmpty) None
      else {
        val interfaceObject = entries.exists(_.isInterfaceObject)
        val directives      = projectDirectives(definition.directives) :::
          entries.flatMap(keyDirective) :::
          (if (interfaceObject) List(Directive("interfaceObject")) else Nil) :::
          contextDeclarations(definition.directives) :::
          composedDirectives(definition.directives)

        def listed(member: String, argument: String): Set[String] =
          ctx
            .join(member, definition.directives)
            .filter(graphArgument(_).contains(graph.key))
            .flatMap { entry =>
              stringArgument(entry.arguments, argument)
            }
            .toSet
        lazy val implemented                                      = listed("implements", "interface")

        val projected: TypeDefinition = definition match {
          case value: ObjectTypeDefinition                       =>
            value.copy(
              implements = value.implements.filter(interface => implemented(interface.name)),
              directives = directives,
              fields = projectFields(value)
            )
          // The graph declared the supergraph interface as an object type.
          case value: InterfaceTypeDefinition if interfaceObject =>
            ObjectTypeDefinition(
              value.description,
              value.name,
              Nil,
              directives,
              projectFields(value)
            )
          case value: InterfaceTypeDefinition                    =>
            value.copy(
              implements = value.implements.filter(interface => implemented(interface.name)),
              directives = directives,
              fields = projectFields(value)
            )
          case value: UnionTypeDefinition                        =>
            // join/v0.2 has no @join__unionMember: a graph then keeps the members it defines.
            val kept: String => Boolean =
              if (ctx.join("unionMember", value.directives).isEmpty) ctx.members(_)(graph.key)
              else listed("unionMember", "member")
            value.copy(directives = directives, memberTypes = value.memberTypes.filter(kept))
          case value: EnumTypeDefinition                         =>
            value.copy(
              directives = directives,
              enumValuesDefinition = projectEnumValues(value.enumValuesDefinition)
            )
          case value: InputObjectTypeDefinition                  =>
            value.copy(directives = directives, fields = projectInputFields(value.fields))
          case value: ScalarTypeDefinition                       =>
            value.copy(directives = directives)
        }

        Some(projected).filterNot(isEmpty)
      }
    }

  private def projectDirectives(directives: List[Directive]): List[Directive] =
    directives.flatMap { directive =>
      ctx.federationDirectiveName(directive.name) match {
        case Some(name) => List(directive.copy(name = name))
        case None       => if (ctx.isFeatureApplication(directive.name)) Nil else List(directive)
      }
    }

  /**
   * Restores this graph's @context declarations under their original names. The namespace
   * identifies the owner when a type carries declarations from several graphs.
   */
  private def contextDeclarations(directives: List[Directive]): List[Directive] =
    declaredContextNames(directives, ctx).flatMap(contextOwner(_, ctx)).collect {
      case (owner, local) if owner.key == graph.key =>
        Directive("context", Map[String, InputValue]("name" -> StringValue(local)))
    }

  /**
   * Restores arguments removed from the supergraph field and recorded in join metadata.
   * Their original positions are unavailable, so append them. SchemaComposer hides these
   * arguments from clients when composing the projected subgraphs.
   */
  private def contextArgumentDefinitions(entry: JoinField): List[InputValueDefinition] =
    entry.contextArguments.flatMap { argument =>
      contextOwner(argument.context, ctx).collect {
        case (owner, local) if owner.key == graph.key =>
          InputValueDefinition(
            description = None,
            name = argument.name,
            // `validate` already proved every declared type parses, so the fallback is unreachable.
            ofType = parseFieldType(argument.contextType).getOrElse(NamedType(argument.contextType, nonNull = false)),
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
  private def composedDirectives(directives: List[Directive]): List[Directive] =
    ctx.join("directive", directives).flatMap { directive =>
      val graphs =
        directive.arguments.get("graphs").toList.flatMap(coercedList).collect { case EnumValue(value) => value }
      val args   = directive.arguments
        .get("args")
        .collect { case InputValue.ObjectValue(fields) => fields }
        .getOrElse(Map.empty[String, InputValue])

      if (graphs.contains(graph.key))
        stringArgument(directive.arguments, "name").map(name => Directive(name.stripPrefix("@"), args)).toList
      else Nil
    }

  /**
   * A field with no `@join__field` belongs to every graph of its type. Otherwise only the named graphs
   * resolve it.
   */
  private def projectFields(value: AggregationTypeDefinition): List[FieldDefinition] =
    for {
      field <- value.fields
      entry <- ctx.entry("field", field.directives, graph.key).map(joinField)
    } yield {
      val translated = List(
        entry.requires.map(fields => Directive("requires", Map("fields" -> StringValue(fields)))),
        entry.provides.map(fields => Directive("provides", Map("fields" -> StringValue(fields)))),
        Some(Directive("external")).filter(_ => entry.external || entry.usedOverridden && entry.overrideLabel.isEmpty),
        entry.overrideFrom.map(from =>
          Directive("override", Map("from" -> StringValue(from)) ++ entry.overrideLabel.map("label" -> StringValue(_)))
        ),
        // Outside the entry: a field with no join entry at all is the default-ownership case, which
        // lands in every member graph and so is the one that most needs declaring shareable.
        Some(Directive("shareable")).filter(_ => ctx.shareable(value.name, field.name, graph.key))
      ).flatten
      // `validate` already proved every declared type parses, so the fallback is unreachable.
      val ofType     = entry.fieldType.flatMap(parseFieldType).getOrElse(field.ofType)

      field.copy(
        ofType = ofType,
        directives = projectDirectives(field.directives) ::: translated ::: composedDirectives(field.directives),
        args = field.args.map(argument => argument.copy(directives = projectDirectives(argument.directives))) :::
          contextArgumentDefinitions(entry)
      )
    }

  private def projectEnumValues(values: List[EnumValueDefinition]) =
    values.collect {
      case value if ctx.entry("enumValue", value.directives, graph.key).nonEmpty =>
        value.copy(directives = projectDirectives(value.directives))
    }

  private def projectInputFields(fields: List[InputValueDefinition]) =
    fields.collect {
      case field if ctx.entry("field", field.directives, graph.key).nonEmpty =>
        field.copy(directives = projectDirectives(field.directives))
    }

  /**
   * Names an operation root only when its type survived projection with at least one field.
   */
  private def projectSchema(definitions: List[Definition]): SchemaDefinition = {
    val populated = definitions.collect { case value: ObjectTypeDefinition =>
      value.name
    }.toSet
    val declared  = ctx.document.schemaDefinition

    def root(name: String): Option[String] = Some(name).filter(populated.contains)

    val query        = root(declared.flatMap(_.query).getOrElse("Query"))
    val mutation     = root(declared.flatMap(_.mutation).getOrElse("Mutation"))
    val subscription = root(ctx.subscriptionRoot)
    val directives   = composedDirectives(DirectiveComposition.schemaDirectives(ctx.document))

    // Even a graph without roots needs the federation link so SchemaComposer recognizes its
    // entity keys and routing directives.
    SchemaDefinition(FederationLink :: directives, query, mutation, subscription, None)
  }
}
