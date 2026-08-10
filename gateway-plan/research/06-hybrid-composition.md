# Hybrid composition: Federation sources plus ordinary GraphQL sources

Research date: 2026-08-07. Comparative revisions: Apollo Router `ce52c982afedb6636e915a2affeb4a27cfbbd53a`, Hive Router `0299232a3e039e2b3cbe2cfb9dbc952f687ab79c`, Cosmo `5edbee289ba54cab1f2e3639b231f1747ead8aa6`, and Hot Chocolate Fusion `00c61af25908319ee277377652191a5aa8c2f60e` (all pinned in [SOURCES.md](../SOURCES.md)). The Composite Schemas working draft was inspected at `da8aa5195700e7e468f0a7d320559519ff271948`.

## Conclusion

The four products do **not** show a majority for a graph-wide choice between “Federation” and “ordinary GraphQL.” They show a stronger and more useful majority rule:

> A gateway may normalize heterogeneous inputs, but every cross-source transition needs explicit identity and recall metadata. It must not guess joins from matching type or field names.

Only Hot Chocolate Fusion makes heterogeneous source protocols a first-class, per-source runtime model. Apollo Router and Hive Router execute Federation supergraphs only. Cosmo Router executes both a single ordinary graph (“monograph”) and federated engine configurations, and its control plane can migrate the former into the latter, but this is conversion into the Federation model rather than concurrent graph-wide stitching semantics. Apollo and Cosmo composition code also accept SDL without a Federation `@link` as Federation v1 input; that compiler permissiveness does not make an arbitrary ordinary endpoint a fully capable Federation subgraph.

For Caliban, keep the agreed single `Gateway.compose(...)` entry point and normalize each input into one internal source model. A Federation adapter supplies Federation identity and `_entities` recall. An ordinary remote or local Caliban source contributes its root fields without annotations; it participates in cross-source entity joins only when Scala configuration declares the key and a normal GraphQL lookup field. This follows Fusion's per-source adapter precedent and the Composite Schemas concepts without claiming full Composite Schemas support.

## What “supporting an ordinary schema” can mean

These cases must not be collapsed into one yes/no feature:

1. **Proxy one ordinary API.** One source, no distributed composition.
2. **Merge disjoint roots.** An ordinary source contributes unique `Query`/`Mutation` fields beside Federation sources; downstream calls remain normal GraphQL.
3. **Merge overlapping types.** Multiple sources contribute to the same type, which requires ownership/shareability rules.
4. **Cross-source entity traversal.** A value obtained from one source is recalled in another. This requires a stable key plus an executable lookup protocol (`_entities` or a normal query field).
5. **Execute in process.** A local schema is called without HTTP. This is an embedding/transport capability, independent of composition semantics.

All four can cover at least the first case. The important divide is that matching SDL names alone is never sufficient for cases 3 and 4.

## Comparative findings

| Product | Native router/runtime input | Composition-tool behavior | Conversion or preprocessing path | What is not natively supported |
| --- | --- | --- | --- | --- |
| **Apollo Router** | Loads a Federation supergraph/JOIN schema; there is no router configuration that directly lists arbitrary ordinary GraphQL APIs. | Apollo's composer treats a schema with no Federation `@link` as Federation v1, and its tests compose such plain-looking SDL together with Federation v2 SDL. | Existing graphs are converted into Federation subgraphs; static SDL can also be supplied to composition. A source with only unique root fields can then receive ordinary GraphQL fetches. | Apollo's documented subgraph contract requires Federation conformance. An unmodified ordinary endpoint does not provide Federation schema acquisition or entity recall, so it cannot be assumed to support `_service`/`_entities` or cross-source entry. |
| **Hive Router** | Loads a Federation v2 supergraph and runs Federation plans. The Rust router does not expose schema-stitching source configuration. | Composition is outside the pinned router. Hive Console treats Single Schema, Schema Stitching, and GraphQL Federation as separate project types. | The wider Guild ecosystem can stitch ordinary schemas with GraphQL Tools/Hive Gateway, but that is a different gateway/runtime, not Hive Router hybrid support. | No evidence that Hive Router consumes a mixed stitching/Federation input or runs per-source non-Federation join protocols. |
| **Cosmo Router** | Can proxy a one-source ordinary **monograph** or execute a generated Federation engine configuration. | The composer classifies schemas without Federation v2 linkage as v1 and can compose v1 and v2 inputs. | `monograph migrate` flips the graph to Federation support while retaining its existing underlying source, after which other labeled subgraphs can be composed. This is an explicit control-plane migration. | A plain source still has no entity-recall protocol merely because it was normalized as v1. Cross-source entry requires Federation keys/entity behavior; otherwise the useful mixed subset is root-level/disjoint routing. |
| **Hot Chocolate Fusion** | First-class per-source connectors. Standard GraphQL sources use normal GraphQL fields; Apollo Federation sources use `_entities`; an in-memory connector executes local schemas directly. | Composition auto-detects Federation v2 per source, translates Apollo directives into Fusion/GraphQL Federation metadata, and can compose Apollo and GraphQL Federation sources in one graph. Plain SDL can be augmented by a separate extensions document. | Source-schema extensions add `@lookup`, `@require`, `@internal`, and related metadata without modifying an upstream schema. | Arbitrary schemas are not magically joinable: cross-source access still needs lookup/key/require metadata and satisfiability validation. |

### Apollo Router

The Router itself is explicit: it is built to run a federated supergraph and requires a supergraph file ([README](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/README.md)). Current Router documentation says its supported artifacts are generated by Federation 2 composition ([Federation version support](https://www.apollographql.com/docs/router/federation-version-support)).

The composition library is more permissive than that runtime contract:

- absence of a Federation link defaults the parsed source to Federation v1 ([`federation_spec_definition.rs`](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-federation/src/link/federation_spec_definition.rs#L1190-L1201));
- composition tests pass a plain `type Query`/`type Product` schema as a v1 source beside a linked Federation v2 source ([`fed1_shareability.rs`](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-federation/tests/composition/fed1_shareability.rs#L1-L115)).

That is input normalization, not a general ordinary-GraphQL connector. Apollo's official subgraph documentation says a source must conform to the Federation specification to belong to a supergraph and describes `_service` and `_entities` as the subgraph protocol ([Subgraphs](https://www.apollographql.com/docs/federation/v1/subgraphs)). Therefore:

- manually supplied plain SDL with unique roots can be compiled into a supergraph and called with ordinary GraphQL operations;
- cross-source entry into that API is unsupported unless the service is actually upgraded to implement the necessary Federation entity contract;
- the Router does not discover, annotate, or stitch arbitrary endpoints by itself.

### Hive Router

The pinned Router describes itself specifically as a GraphQL Federation router, and its only graph input is a supergraph ([README](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/README.md#L1-L39)). Its official compatibility page targets Federation v2 and rejects Federation v1 supergraph artifacts ([compatibility](https://the-guild.dev/graphql/hive/docs/router/compatibility)).

Hive as a platform supports three separate registry models—Single Schema, Schema Stitching, and GraphQL Federation—and applies the selected model's composition rules ([Hive schema registry](https://the-guild.dev/graphql/hive/docs/schema-registry)). GraphQL Tools can stitch arbitrary local or remote schemas and configure type merging ([Schema Stitching](https://the-guild.dev/graphql/stitching/docs), [type merging](https://the-guild.dev/graphql/stitching/docs/approaches/type-merging)), but this capability belongs to GraphQL Tools/Hive Gateway. There is no evidence in the pinned Rust Router of a corresponding stitching input or execution path.

Consequently, “Hive supports both” is true of the product family, but **not** of Hive Router as one hybrid runtime. For this comparison, Hive Router is Federation-only and ordinary-schema integration requires a different gateway or an external transformation into a Federation supergraph.

### Cosmo Router

Cosmo explicitly supports a **monograph**, defined as a graph with Federation disabled and exactly one underlying source ([monograph overview](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/docs-website/cli/monograph.mdx), [creation](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/docs-website/cli/monograph/create.mdx)). This is native ordinary-API proxying, not multi-source stitching.

Cosmo also provides an explicit migration to a federated graph. The implementation locates a graph with `supportsFederation: false` and calls `enableFederationSupport` ([`migrateMonograph.ts`](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/controlplane/src/core/bufservices/monograph/migrateMonograph.ts#L15-L75)); the CLI documentation says labels can then be changed to compose with other graphs ([migration](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/docs-website/cli/monograph/migrate.mdx)).

At composition level, an unlinked schema is classified as Federation v1. Tests use plain-looking SDL without `@link` and assert `isVersionTwo == false`, while other fixtures are v2 ([`federation-factory.test.ts`](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/composition/tests/v1/federation-factory.test.ts#L577-L591), [fixture](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/composition/tests/v1/federation-factory.test.ts#L1457-L1465)).

This makes Cosmo a deliberate example of **upgrade/normalization**: an existing ordinary graph can become the first source of a federation. It does not show a second, arbitrary stitching protocol inside the Federation plan. Unique roots work naturally; cross-source traversal still needs keys and an entity-capable downstream contract.

### Hot Chocolate Fusion

Fusion is the direct precedent for the requested model. Its documentation says standard GraphQL servers can act as sources and characterizes source schemas independently of transport ([Fusion introduction](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/website/content/docs/fusion/index.md#L1-L42)). It then handles heterogeneity per source:

- the Apollo Federation connector detects Federation v2 SDL during composition, translates keys/requires into its internal model, and speaks `_entities` only to those sources at runtime;
- Apollo Federation and GraphQL Federation sources can coexist in one graph ([connector documentation](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/website/content/docs/fusion/connectors/apollofederation.md#L1-L38));
- an extensions document layers lookup and requirement metadata over a base schema that cannot be edited ([source-schema extensions](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/website/content/docs/fusion/source-schema-extensions.md#L1-L75));
- its in-memory connector executes operations against an in-process request executor rather than HTTP ([registration](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Connectors.InMemory/DependencyInjection/InMemoryFusionGatewayBuilderExtensions.cs#L13-L112), [client](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Connectors.InMemory/InMemorySourceSchemaClient.cs#L19-L70)).

Fusion does not infer arbitrary joins. Plain root fields need no special protocol, but extending entities across sources requires explicit lookup/key metadata and the composer runs satisfiability validation.

## Minimal Composite Schemas concepts worth adopting

The current [GraphQL Composite Schemas working draft](https://graphql.github.io/composite-schemas-spec/draft/) is still **Stage 0: Preliminary** at commit [`da8aa519`](https://github.com/graphql/composite-schemas-spec/tree/da8aa5195700e7e468f0a7d320559519ff271948). It explicitly does not claim that arbitrary schemas are directly composable; tooling may transform external schemas into conforming source schemas ([overview](https://github.com/graphql/composite-schemas-spec/blob/da8aa5195700e7e468f0a7d320559519ff271948/spec/Section%201%20--%20Overview.md)). Caliban should borrow its model while avoiding a conformance claim.

The smallest useful semantic slice is:

1. **Source identity and field ownership.** Every type/field contribution records its source. Collisions are errors unless equivalence is explicit.
2. **`@key` concept: stable entity identity.** A selection identifies a value across sources.
3. **`@lookup` concept: entity recall.** A target source exposes a normal Query field capable of resolving that entity ([source-schema directives](https://github.com/graphql/composite-schemas-spec/blob/da8aa5195700e7e468f0a7d320559519ff271948/spec/Section%202%20--%20Source%20Schema.md)).
4. **`@is` concept: argument mapping.** Lookup arguments can map to differently named or nested key fields rather than relying on name equality.
5. **`@shareable` concept: explicit equivalent providers.** Duplicate ownership is not inferred.
6. **Merge followed by satisfiability.** Every exposed path must have at least one executable plan ([composition](https://github.com/graphql/composite-schemas-spec/blob/da8aa5195700e7e468f0a7d320559519ff271948/spec/Section%204%20--%20Composition.md)).

`@require`-style argument injection is the next valuable increment because it supports ordinary APIs whose field arguments come from parent data. `@external`, `@provides`, `@internal`, and `@inaccessible` remain important, but basic root coexistence and key/lookup joins do not require implementing all of them at once. Federation `@requires` must not be mechanically translated to Composite `@require`: their directive locations and detailed semantics differ.

## Recommended Caliban model

Retain one graph-level API; make semantics explicit on each source and each cross-source edge:

```scala
Gateway.compose(
  Subgraph.federated("products", productsUrl),
  Subgraph
    .graphql("reviews", reviewsUrl)
    .lookup[Product](key = _.id)(field = "productById", argument = "id"),
  Subgraph.local("inventory", inventoryApi)
)
```

The precise Scala syntax remains a later API-design decision. The semantic rules should be fixed now:

- **No graph-wide mode.** `Gateway.compose(...)` accepts heterogeneous source descriptions.
- **Normalize before composing.** Federation, ordinary remote GraphQL, and local Caliban adapters all produce the same internal source-schema model.
- **Automatic root coexistence.** Unique ordinary root fields may coexist with Federation roots without pretending the ordinary server implements Federation.
- **Explicit joins.** A shared type name does not create an edge. Cross-source entry needs a declared key plus either Federation `_entities` capability or a normal/local lookup resolver.
- **Per-source execution capability.** Plans carry the source operation kind (`FederationEntities`, `GraphQLQuery`, or in-process Caliban), not a graph-wide Federation/stitching flag.
- **Validate satisfiability.** Composition rejects an exposed field that cannot be reached from every relevant origin.
- **Stage the claim.** Call the first implementation “Composite-inspired heterogeneous composition,” not “Composite Schemas support.”

This accommodates the requested hybrid graph from the start, preserves the single API the user prefers, and remains ready to map more of the Composite Schemas specification later without making that unstable draft the v1 public contract.
