// Composes `supergraph-hive.graphql` from the same two subgraphs `supergraph.yaml` declares, using
// Hive's composer instead of rover's.
//
// Development-time only: nothing in `gateway/test` runs this, so the suite does not depend on npm
// or on byte-stable composer output. See README.md for the recipe and for what the two composers
// are known to disagree about.

import { readFileSync, writeFileSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { parse } from 'graphql'
import { assertCompositionSuccess, composeServices } from '@theguild/federation-composition'

const here = dirname(fileURLToPath(import.meta.url))
const read = (name) => readFileSync(join(here, name), 'utf8')

// These must match `supergraph.yaml`. Unlike rover, which rejects a subgraph with no `routing_url`,
// `composeServices` defaults a missing `url` to the empty string and composes anyway — and a
// supergraph whose `@join__graph` urls are all empty decomposes to different endpoints than the
// rover fixture, which is the whole comparison this file exists for.
const subgraphs = [
  { name: 'characters', schema: 'characters.graphql', url: 'http://127.0.0.1:9008/graphql/federated' },
  { name: 'episodes', schema: 'episodes.graphql', url: 'http://127.0.0.1:9009/graphql/federated' }
]

const result = composeServices(
  subgraphs.map(({ name, schema, url }) => ({ name, url, typeDefs: parse(read(schema)) }))
)

// Throws with the composition errors rather than writing an unusable fixture.
assertCompositionSuccess(result)

const output = 'supergraph-hive.graphql'
writeFileSync(join(here, output), result.supergraphSdl)

for (const { name, url } of subgraphs) console.log(`  ${name} -> ${url}`)
console.log(`wrote ${output}`)
