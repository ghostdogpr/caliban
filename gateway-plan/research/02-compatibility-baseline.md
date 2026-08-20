# GraphQL and Federation compatibility baseline

Research date: 2026-08-07. Comparative source revisions are pinned in [SOURCES.md](../SOURCES.md): Apollo Router `ce52c982afed`, Hive Router `0299232a3e03`, Cosmo `5edbee289ba5`, and Hot Chocolate Fusion `00c61af25908`.

## Decision

The first production-capable Caliban Graph Router should be compatible at three explicit boundaries:

1. **Client boundary:** conform to the [September 2025 GraphQL specification](https://spec.graphql.org/September2025/) and the JSON profile of the Stage 2 [GraphQL-over-HTTP specification](https://graphql.github.io/graphql-over-http/draft/). Preserve `application/json` behavior for legacy clients, but implement `application/graphql-response+json` negotiation and its status-code semantics rather than copying one existing router's legacy quirks.
2. **Federation source boundary:** compose Federation 2 subgraph schemas through the Federation 2.9 directive set, but make the first unary release gate the features supported by a majority of the four implementations: `@link`, `@key`, `@external`, `@requires`, `@provides`, `@shareable`, basic `@override`, `@inaccessible`, `@tag`, `@composeDirective`, entity interfaces, and `@interfaceObject`. Composition must apply the published merge and satisfiability rules, not merely recognize directive names.
3. **Execution-artifact boundary:** the semantic model must faithfully represent the current released [JOIN v0.3 specification](https://specs.apollo.dev/join/v0.3/). Accepting a valid JOIN v0.3 supergraph as an alternate input is part of v1 interoperability. The optimized runtime artifact may be a versioned Scala-specific format; it must not leak into client behavior or lose information needed to export/diagnose JOIN semantics.

Federation 2.8 contexts (`@context`/`@fromContext`, represented by de-facto JOIN v0.5), progressive override labels (de-facto JOIN v0.4), subscriptions, and incremental delivery are **designed now but delivered after unary v1**. Uploads, client request batching, APQ/persisted operations, and exact Apollo error wording are optional compatibility profiles, not the semantic baseline.

This choice follows the majority without freezing the design to the lowest common denominator. Apollo and Hive consume JOIN supergraphs; Cosmo and Fusion compile different private artifacts. All four nevertheless converge on the same hot-path concepts: validate against an API schema, select fields by source, inject key/require selections, issue batched entity lookups, run independent query work concurrently, preserve top-level mutation order, merge by response path, and complete the final value against GraphQL nullability.

## Normative unary release gate

### GraphQL language and execution

The router is itself the GraphQL service seen by clients. Delegating to conforming subgraphs does not discharge its obligations. The unary gate therefore includes:

- the complete September 2025 executable grammar; fragment and operation selection; all standard validation rules; variables, literals, defaults, list/input-object coercion, custom scalars, `@oneOf`, and `@skip`/`@include`;
- aliases, repeated-field collection, fragment/type-condition applicability over objects/interfaces/unions, and introspection of the **API schema**, never federation execution metadata;
- parallel execution where GraphQL permits it, but serial execution of top-level mutation fields in client document order even when adjacent fields target different sources;
- final response completion against the API type, including enum/runtime-type validation, list element positions, field order, and Non-Null bubbling to the nearest nullable ancestor;
- GraphQL response shape: `data`, `errors`, and optional `extensions`; request errors omit `data`; executed field errors include a path and coexist with partial `data`; error paths use client aliases and list indices, not `_entities`, generated aliases, or other subgraph-plan paths.

These are normative GraphQL requirements. The compared implementations reinforce the distributed cases: Hive explicitly rewrites mutation fetch roots into document-order sequence ([source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/query-planner/src/planner/fetch/optimize/turn_mutations_into_sequence.rs#L12-L64)) and has end-to-end null/error propagation cases over roots, nested objects, and lists ([suite](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/e2e/src/issues/mod.rs#L1274-L1420)); Apollo tests rewriting entity error paths back into client paths ([suite](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-router/tests/integration/query_planner/error_paths.rs)); Fusion performs value completion over its merged result store ([source](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Execution/Execution/Results/ValueCompletion.cs)).

### GraphQL over HTTP

For the public endpoint, v1 must:

- accept `POST` with UTF-8 `application/json` and the standard `query`, `operationName`, `variables`, and `extensions` members;
- accept `GET` for query operations with those values URL encoded; reject a selected mutation over `GET` with `405` and `Allow: POST`;
- reject malformed transport input before GraphQL execution (unsupported content type, invalid JSON, wrong parameter shapes, body/URI limits) with an appropriate `4xx`;
- negotiate `application/graphql-response+json` and legacy `application/json`; use `2xx` whenever non-null `data` exists and `4xx`/`5xx` for request/server failures without `data` under the GraphQL response media type; executed field errors remain `200`;
- always send subgraph GraphQL queries and mutations as POST JSON in unary v1. A source adapter may later support alternatives without changing plan semantics.

The GET/405 behavior is both specified and common: Apollo has a dedicated enforcement layer that returns `405` and `Allow: POST` ([source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-router/src/services/layers/allow_only_http_post_mutations.rs#L84-L130)); Cosmo tests successful GET queries, URL-encoded variables, malformed parameters, and mutation rejection ([suite](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router-tests/protocol/graphql_over_get_test.go#L19-L134)).

Status codes are the main protocol divergence. Existing routers often preserve pre-spec `application/json` behavior: Cosmo, for example, returns `400` for a malformed `variables` shape but `200` for missing or uncoercible required variables ([suite](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router-tests/protocol/integration_test.go#L561-L619)). Caliban should not make such differences global constants. Status selection is a function of the negotiated response media type, with an optional legacy/Apollo profile if migration evidence requires it.

### Federation composition

The offline composer must:

- resolve `@link` feature URLs, versions, namespace aliases, imports, and renamed directive imports; reject unsupported major versions and uses introduced after the declared Federation version;
- validate every `FieldSet` structurally and against its source type, including nested and composite keys, repeatable keys, `resolvable: false`, and valid `@requires`/`@provides`/`@external` relationships;
- merge objects, interfaces, and unions by union; input fields and arguments by intersection; merge enums by union in output-only positions, intersection in input-only positions, and exact agreement when used in both. Reject incompatible required input members and field/argument types;
- enforce field ownership/shareability, override source validity, inaccessible-reference closure, interface implementation, entity-interface/`@interfaceObject` projection, and graph satisfiability (every API operation must have a valid execution route);
- preserve composed custom directives selected by `@composeDirective` and the security metadata `@authenticated`, `@requiresScopes`, and `@policy` in the artifact;
- emit deterministic diagnostics with a stable machine code, schema/coordinate, severity, and human message. Apollo wording is not a compatibility requirement.

The merge strategies and resolvability rule are specified in Apollo's official [composition rules](https://www.apollographql.com/docs/graphos/schema-design/federated-schemas/reference/composition-rules). The Federation subgraph contract also defines `_service`, `_entities`, representation validation, and stable result ordering ([subgraph specification](https://www.apollographql.com/docs/graphos/schema-design/federated-schemas/reference/subgraph-spec)).

Authorization execution itself remains outside this effort's built-in policy scope. Silently ignoring composed security directives would be unsafe: a graph containing them must either be rejected for production activation or require an installed policy decision hook. The baseline only requires lossless composition, a fail-closed activation rule, and a narrow execution hook that can later implement filtering/rejection.

### Federation planning and execution

The unary execution gate includes:

- root-field routing and coalescing compatible selections for the same source into one sub-operation when dependencies permit;
- deterministic source choice for shareable fields; correctness must not depend on one particular choice;
- entity transitions by collecting `__typename` plus one satisfiable key, batching representations into one `_entities` call per compatible step/source, and correlating nullable results to representations in exact input order;
- `@requires` dependency fetches (including nested fields) before the dependent field, and `@provides` satisfaction without a redundant entity round trip when the providing path covers the selection;
- basic `@override` as exclusive ownership by the destination source; inaccessible elements remain usable for execution (for example in keys) but absent from the API schema;
- aliases, fragments, abstract types, lists/nested lists, conditional `@skip`/`@include`, partial entity misses, and multiple keys across multi-hop plans;
- bounded parallel query branches and fail-fast dependency cancellation, while preserving serial root mutation semantics;
- mapping every subgraph data/error path into the client response path before final GraphQL completion.

Remote subgraph GraphQL errors are path-rewritten and redacted by default, with source locations omitted and extension keys allowlisted (`code` by default); operators can opt into messages and additional extension keys globally or per source. Local Caliban errors retain Caliban behavior. Transport/protocol failures become safe router-authored field errors. A subgraph HTTP status is never copied to the client HTTP status after GraphQL execution has produced partial data. Apollo defaults to redacting subgraph messages and extensions ([source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-router/src/plugins/include_subgraph_errors/config.rs#L11-L50)); Cosmo defaults to path rewriting and an extension allowlist ([source](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router/pkg/config/config.go#L1073-L1092)).

## Comparative semantic matrix

Legend: **Yes** = direct support/evidence in the pinned source or suite; **Partial** = supported with a narrower version/profile or non-equivalent artifact; **No** = deliberately absent or no execution support in the pinned implementation. This is a compatibility comparison, not a certification of every edge case.

| Capability | Apollo Router | Hive Router | Cosmo Router | Hot Chocolate Fusion | Caliban baseline |
| --- | --- | --- | --- | --- | --- |
| GraphQL query/mutation validation, variables, fragments, abstract types, introspection | Yes | Yes | Yes | Yes | Unary v1, GraphQL Sep-2025 |
| GET query; mutation-over-GET rejected | Yes | Yes | Yes | Yes (Hot Chocolate host) | Unary v1 |
| `application/graphql-response+json` plus legacy JSON | Yes/legacy quirks | Yes/legacy quirks | Partial; public tests commonly emit `application/json` | Yes | Unary v1, spec profile + legacy profile |
| Correct alias-aware errors and Non-Null/list completion across sources | Yes; completion violations may additionally use `extensions.valueCompletion` | Yes | Yes; Apollo compatibility mode can use `valueCompletion` | Yes | Unary v1; normative `errors` + bubbling |
| Query concurrency; serial top-level mutations | Yes | Yes | Yes | Yes | Unary v1 |
| Federation subgraph composition bundled with product | Yes (federation library/Rover ecosystem) | No in router snapshot; consumes artifact | Yes | Yes | Separate library/CLI in v1 |
| Consumes JOIN supergraph | Yes, v0.1-v0.5 in source | Yes, v0.3 fields plus v0.4 override labels | No; consumes generated engine JSON | No; consumes Fusion archive/schema | JOIN v0.3 import; private optimized artifact |
| `@key` (repeatable/nested/resolvable), `_entities`, multi-hop entity joins | Yes | Yes | Yes | Yes via Federation-to-Fusion transform | Unary v1 |
| `@requires` / `@provides` | Yes | Yes | Yes | Yes | Unary v1 |
| `@shareable`, basic `@override` | Yes | Yes | Yes | Yes | Unary v1 |
| `@inaccessible`, `@tag`, `@composeDirective` | Yes | Yes | Yes | Yes/preserved through transform | Unary v1 |
| Entity interfaces / `@interfaceObject` (Fed 2.3) | Yes | Yes | Yes | Yes | Unary v1 |
| `@authenticated`, `@requiresScopes`, `@policy` metadata | Yes, built-in enforcement | Yes, built-in enforcement | Yes, built-in enforcement | Partial/different authorization model | Preserve + fail-closed policy hook; built-in policy later |
| Progressive override label / JOIN 0.4 | Yes | Yes | No evidence | No evidence | Later compatibility milestone |
| `@context` / `@fromContext` / JOIN 0.5 | Yes | No evidence (`contextArguments` absent) | No evidence | No evidence | Later compatibility milestone |
| Federated subscriptions | Yes | Yes | Yes | Yes | Designed now, delivered next |
| Federated `@defer` | Yes | No (suite explicitly waits for protocol support) | Yes | Yes | Designed now, delivered next |
| Federated `@stream` | Partial/planner scaffolding | No | No evidence | Partial/detection scaffolding | Later than `@defer`, gated by interop tests |
| Multipart file upload and client request batching | Optional/configurable | Optional | Optional/configurable | Yes | Not in semantic v1 gate |

Evidence for the Federation rows:

- Apollo explicitly registers JOIN 0.1 through 0.5 and records what 0.3, 0.4, and 0.5 add ([source](https://github.com/apollographql/router/blob/ce52c982afedb6636e915a2affeb4a27cfbbd53a/apollo-federation/src/link/join_spec_definition.rs#L1247-L1280)). Only JOIN v0.3 is currently listed as a released spec in Apollo's [specification index](https://specs.apollo.dev/); 0.4/0.5 are therefore compatibility extensions, not the initial normative interchange floor.
- Hive's JOIN field representation covers `requires`, `provides`, source type, external/override, and progressive `overrideLabel`, but not context arguments ([source](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/query-planner/src/federation_spec/join_field.rs#L10-L19), [parser](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/lib/query-planner/src/federation_spec/join_field.rs#L93-L137)).
- Cosmo's composition suite has dedicated cases for authorization, composed directives, field sets, inaccessible, interface objects, override, provides, shareable, and one-of inputs ([suite directory](https://github.com/wundergraph/cosmo/tree/5edbee289ba54cab1f2e3639b231f1747ead8aa6/composition/tests/v1/directives)); its runtime consumes a generated engine configuration rather than JOIN SDL.
- Fusion detects Federation v2 via its `@link`, validates it, removes Federation infrastructure, generates lookups, rewrites keys/requires, and removes external fields into its own source model ([transformer](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Composition/ApolloFederation/FederationSchemaTransformer.cs#L13-L105)). Its recognized directive list covers the unary core and security metadata but not contexts ([source](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/src/Fusion.Composition/ApolloFederation/FederationDirectiveNames.cs#L3-L20)).
- Hive explicitly marks end-to-end `@defer` testing as waiting for multipart protocol support ([test](https://github.com/graphql-hive/router/blob/0299232a3e039e2b3cbe2cfb9dbc952f687ab79c/e2e/src/demand_control/estimator.rs#L1090-L1098)); Apollo, Cosmo, and Fusion have defer execution suites, including Cosmo's `Accept: multipart/mixed` requirement ([Cosmo](https://github.com/wundergraph/cosmo/blob/5edbee289ba54cab1f2e3639b231f1747ead8aa6/router-tests/protocol/defer_test.go#L205-L235), [Fusion](https://github.com/ChilliCream/graphql-platform/blob/00c61af25908319ee277377652191a5aa8c2f60e/src/HotChocolate/Fusion/test/Fusion.AspNetCore.Tests/DeferTests.cs)).

## Explicit divergence resolutions

1. **HTTP status and media type:** follow GraphQL-over-HTTP when `application/graphql-response+json` is negotiated; offer a separate legacy profile. Do not hard-code Apollo's or Cosmo's historical status choices.
2. **Error text and extension codes:** response shape, path, nulling, and classification are compatibility; English wording, ordering of independent errors, and vendor extension codes are not. Stable Caliban codes belong to router-authored errors only.
3. **Subgraph errors:** rewrite to client paths, redact remote messages by default, and allowlist remote extensions, with explicit global and per-source disclosure controls. Never expose generated `_entities` paths or blindly trust arbitrary extensions; local Caliban errors keep local semantics.
4. **Invalid subgraph values:** perform final router-side completion. Apollo's `extensions.valueCompletion` may be offered as a profile, but normative execution errors must appear in `errors` and trigger the required null propagation.
5. **Shareable selection and plan shape:** plan identity is not observable compatibility. Any deterministic, valid source choice is allowed; tests assert results and source-call invariants, not byte-identical Apollo plans.
6. **Artifacts:** JOIN v0.3 is the portable semantic interchange. An optimized Scala artifact is allowed and desirable, but must be versioned, validated, deterministic, and lossless with respect to v1 semantics.
7. **Advanced Federation:** accept neither contexts nor progressive-override labels silently in unary v1. Composition reports an unsupported-feature diagnostic until the corresponding planner/executor capability is enabled.
8. **Security directives:** never compose successfully and then ignore enforcement. Activation fails closed without an installed policy hook.

## Compatibility acceptance suite

Release should be blocked on black-box fixtures, not implementation resemblance:

- import the GraphQL Foundation conformance cases applicable to September 2025 and add gateway-specific distributed variants for aliases, fragments, coercion, introspection, mutation order, nested lists, and error bubbling;
- run a GraphQL-over-HTTP matrix across POST/GET, request shapes, `Accept`/`Content-Type`, malformed JSON, parse/validation/coercion/field errors, and both response media profiles;
- compose positive and negative Federation fixtures for every unary directive, merge strategy, field-set rule, satisfiability failure, renamed import/namespace, and entity-interface projection;
- execute the same semantic corpus against remote GraphQL sources and in-process Caliban sources, including mixed graphs, multiple keys, nested `@requires`, `@provides`, shareable alternatives, inaccessible keys, abstract entities, nullable entity misses, and subgraph protocol failures;
- differential-test result JSON and subgraph-call invariants against all four pinned gateways where each supports the feature. Normalize vendor messages/codes and nondeterministic independent-error order; do not normalize `data`, client paths, null propagation, mutation order, HTTP classification, or missing/extra source calls;
- keep subscriptions and incremental fixtures in the suite from day one but mark them expected-unsupported until their staged milestone. This prevents the unary plan/artifact model from erasing stream lifetimes, patch paths, cancellation, or artifact-retirement needs.

**Audit qualification.** The pinned Federation Gateway Audit is a useful seed for the distributed-execution bullet, not a substitute for this acceptance suite. It supplies 199 operations across 46 suites, but its normal adapter path executes an Apollo-composed supergraph, 187 cases do not assert that errors are absent, only error presence is checked in the other 12, and it does not assert HTTP status/media type or full GraphQL errors. Its checked-in aggregate report is also stale relative to the checked-in Hot Chocolate 199/199 result. Import its schemas, operations, and expected `data`; fork the runner to add complete response/HTTP assertions and run both JOIN-import and Caliban-native composition modes. See [Cross-gateway audit and benchmark as acceptance evidence](17-cross-gateway-audit-and-benchmark.md).

## Staging summary

**Unary v1:** standalone single-source GraphQL plus mixed remote/local Federation graphs; queries and mutations; GraphQL September 2025; GraphQL-over-HTTP JSON; offline Federation composition; JOIN v0.3 import semantics; core Federation through entity interfaces; safe error propagation; spec-correct final completion.

**Streaming milestone:** subscriptions first (GraphQL response streams, source protocol negotiation, cancellation/backpressure, per-event enrichment), then `@defer` with the current incremental response format and multipart negotiation. Treat `@stream` separately because the comparison does not show majority production execution support.

**Later compatibility profiles:** progressive overrides/JOIN 0.4, contexts/JOIN 0.5, built-in Federation authorization policies, uploads, client batching, APQ/persisted-operation protocols, and Apollo-specific error/status/value-completion profiles.
