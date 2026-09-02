# Supergraph decomposition fixture

`supergraph.graphql` is the input to `SupergraphDecompositionSpec`. `characters.graphql` and
`episodes.graphql` are the subgraphs it was composed from, so the round-trip test can compose them
directly and compare against the decomposed-then-composed result.

`context-supergraph.graphql` is a second, smaller supergraph, composed from
`context-characters.graphql` and `context-episodes.graphql`, that exercises `@context` and
`@fromContext`. It is separate because the composer does not merely relabel those: it deletes every
context argument from the field it belongs to and namespaces every context name by the subgraph that
declared it, so the round trip is the only thing that proves the arguments come back in a shape
`SchemaComposer` accepts.

`supergraph-hive.graphql` is the **same two subgraphs composed by Hive's composer** rather than
rover's. It exists because Hive composes with `@theguild/federation-composition`, so a supergraph
published through Hive is not necessarily byte-comparable to a rover one, and `SupergraphDecomposition`
has only ever been fed rover output.

## Regenerating

Composition is a **development-time** step. Nothing in the test suite shells out to rover — that
would make `gateway/test` depend on rover being installed and on byte-stable output across releases.

```sh
cd gateway/src/test/resources/supergraph
rover supergraph compose --config ./supergraph.yaml --elv2-license accept > /tmp/composed.graphql
diff /tmp/composed.graphql supergraph.graphql
```

Verified with rover 0.40.0 and the `supergraph-v2.10.0` composer plugin, which rover downloads on
first use. The output is byte-identical to the checked-in `supergraph.graphql`, with no
normalization.

`federation_version` is pinned in `supergraph.yaml` and matters. All of 2.10, 2.11 and 2.12 emit
`join/v0.5`, but **2.9 differs**: it writes `import: ["@inaccessible"]` into the `inaccessible`
`@link`, which the checked-in fixture does not carry. If a future composer changes the output,
prefer pinning to a version that reproduces the fixture over regenerating it — the spec asserts
against this document, including exact diagnostic strings.

## Regenerating the context fixture

```sh
cd gateway/src/test/resources/supergraph
rover supergraph compose --config ./context-supergraph.yaml --elv2-license accept > /tmp/composed.graphql
diff /tmp/composed.graphql context-supergraph.graphql
```

Two constraints shape those subgraphs, and both fail confusingly if broken:

- **A context field must only be reachable *through* its context type.** `Ship` is reachable solely
  via `Character.ship`, and adding a `Query.ships` root makes rover reject the whole composition
  with `SATISFIABILITY_ERROR: could not find a match for required context`.
- **Every subgraph needs a query root.** `Query.shipCount` exists only so the `episodes` projection
  has one; `RemoteSchema.normalize` refuses a document without it, so the round trip dies with
  `[episodes] The query root operation is missing.` rather than reporting anything about contexts.

No Hive counterpart is checked in: composing the same subgraphs with
`@theguild/federation-composition` 0.26.0 produces the same encoding of both halves — the same
`@context(name: "<subgraph>__<name>")` applications and the same `contextArguments` entries, down to
the leading space in `selection` — and `supergraph-hive.graphql` already covers the printing and
ordering differences between the two composers.

## Regenerating the Hive fixture

Same development-time rule: nothing in the test suite shells out to npm. `compose-hive.mjs` composes
the two subgraphs and writes `supergraph-hive.graphql` back.

```sh
cd gateway/src/test/resources/supergraph
npm install --no-save @theguild/federation-composition@0.26.0 graphql
node compose-hive.mjs
git diff supergraph-hive.graphql
```

`node_modules/` and `package-lock.json` are already git-ignored repo-wide, so the install leaves
nothing to clean up. Verified with `@theguild/federation-composition` 0.26.0 on node 24; the output
is byte-identical to the checked-in fixture, with no normalization. Do not reformat it — the
composer emits two-space indentation and an editor that reindents on save breaks the `git diff`
check above.

**The routing urls have to be passed explicitly.** Rover rejects a subgraph with no `routing_url`;
`composeServices` defaults a missing `url` to `""` and composes anyway, which yields a supergraph
whose `@join__graph` entries carry no endpoints at all. That decomposes to a different graph set
than the rover fixture and makes the two incomparable, so `compose-hive.mjs` repeats the same two
urls `supergraph.yaml` declares. Keep them in sync.

## How the two composers differ, as of these versions

Both emit `join/v0.5`, and the two supergraphs agree on all 24 top-level definitions. Rover emits
two more that Hive's composer does not:

- `directive @join__directive(...)`
- `scalar join__DirectiveArguments`

Neither fixture uses either one, so this is join-spec machinery rover writes unconditionally.

The remaining differences are ordering and printing, not content: rover sorts definitions and puts
each directive's arguments on one line, Hive emits them in a different order and breaks long argument
lists across lines. A comparison between the two fixtures therefore has to be made over the decomposed
graph set, not over the text.

## What the fixture is built to exercise

- `Episode.season` / `Episode.episode` — key fields owned by **both** graphs, carrying no
  `@join__field`. This is the default-ownership path, and the case that must gain `@shareable`.
- `Character.name` — owned by `characters`, `@external` in `episodes`, so it is a single-provider
  field that must **not** gain `@shareable`.
- `Character.isCaptain` — owned by `episodes`, while `Episode.characters` is `characters`-only.
  `Character` is therefore unreachable from the `episodes` `Query` and survives composition only
  through orphan promotion, which is enabled only when the synthesized federation `@link` is
  present. It is the tightest coupling between the `@link` and entity routing.
- `Character.biography` — `@inaccessible`, which is a real federation directive and must survive
  decomposition rather than being stripped with the join machinery.
- `Role` / `Origin` — per-graph union members and enum values.
- `Query.episodes(seaons: Int)` — the argument name is misspelled in the source subgraph. It is
  preserved deliberately; correcting it changes the fixture.

## What the context fixture is built to exercise

- `Character` carries **both** graphs' declarations, `@context(name: "characters__viewer")` and
  `@context(name: "episodes__crew")`, side by side. The namespace is the only thing that says which
  graph a declaration belongs to, and it is built from the subgraph *name* — the graph enum key is
  `CHARACTERS`, so a projection filtered by key silently emits no contexts at all.
- `Ship.fare` — two context arguments plus a surviving ordinary one. The subgraph declares them as
  `fare(currency: ..., size: Int!, locale: ...)`, and only `size` reaches the supergraph, so the
  projection cannot restore the authored order. Nothing can see the difference: `SchemaComposer`
  hides every context argument from the composed field.
- `Ship.manifest` — a selection carrying a type condition, `$crew ... on Character { rank }`, which
  the supergraph stores as the bare ` ... on Character { rank }` with the context name split off.
- `episodes` declares its context on `Character`, which is unreachable from its own `Query` —
  `shipCount` is that graph's only root field — so the projection needs orphan promotion on top of
  the context metadata.
