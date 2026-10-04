# Caliban Federation Gateway Audit adapter

This non-published project runs the pinned Federation Gateway Audit against Caliban's gateway. `install.sh` builds the adapter and installs it into an audit checkout as `gateways/caliban`, with the same `run.sh` and `gateway.json` as the upstream entry. For each suite, `run.sh` writes the suite's supergraph, and the adapter serves `Gateway.fromSupergraph` over it.

The canonical upstream repository, reviewed commit, and review date live in [`upstream.env`](upstream.env). CI runs the
unmodified upstream reporter and requires every reported case to pass. `verify-results.sh` rejects failures, duplicate
cases, and missing or inconsistent result summaries.

## Prepare

Install Node.js, sbt, and a JDK. Then check out the pinned upstream repository:

```sh
CALIBAN_ROOT=/path/to/caliban
. "$CALIBAN_ROOT/gateway-audit/upstream.env"
git clone "$FEDERATION_GATEWAY_AUDIT_REPOSITORY" /path/to/federation-gateway-audit
git -C /path/to/federation-gateway-audit checkout "$FEDERATION_GATEWAY_AUDIT_REVISION"
npm --prefix /path/to/federation-gateway-audit ci --ignore-scripts
```

## Run

Install the adapter into the upstream checkout, then run the audit there:

```sh
"$CALIBAN_ROOT/gateway-audit/install.sh" /path/to/federation-gateway-audit
make -C /path/to/federation-gateway-audit test-caliban
"$CALIBAN_ROOT/gateway-audit/verify-results.sh" /path/to/federation-gateway-audit/gateways/caliban/results.txt
```

## Ports

The adapter serves `/graphql` on port 4000. The audit serves its fixtures on port 4200.
