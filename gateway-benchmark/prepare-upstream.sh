#!/bin/sh
set -eu

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <graphql-gateway-benchmarks-checkout>" >&2
    exit 1
fi

SCRIPT_DIR=$(CDPATH= cd -P -- "$(dirname -- "$0")" && pwd)
UPSTREAM_DIR=$(CDPATH= cd -- "$1" && pwd)
. "$SCRIPT_DIR/upstream.env"
EXPECTED_REVISION=$GRAPHQL_GATEWAY_BENCHMARKS_REVISION
ACTUAL_REVISION=$(git -C "$UPSTREAM_DIR" rev-parse HEAD)

if [ -z "$EXPECTED_REVISION" ] || [ "$ACTUAL_REVISION" != "$EXPECTED_REVISION" ]; then
    echo "Expected ChilliCream benchmark revision $EXPECTED_REVISION, found $ACTUAL_REVISION." >&2
    exit 1
fi

if ! git -C "$UPSTREAM_DIR" diff --quiet -- k6 apollo-federation/subgraphs-rust/main.rs apollo-federation/subgraphs-rust/start.sh; then
    echo "The pinned benchmark driver or subgraph sources have tracked changes; restore them before running the benchmark." >&2
    exit 1
fi

ADAPTER_DIR="$UPSTREAM_DIR/apollo-federation/gateways/caliban"
mkdir -p "$ADAPTER_DIR"
ln -sfn "$SCRIPT_DIR" "$ADAPTER_DIR/caliban-adapter"
ln -sfn "$SCRIPT_DIR/target" "$ADAPTER_DIR/target"
cat > "$ADAPTER_DIR/install.sh" <<'WRAPPER'
#!/usr/bin/env bash
set -euo pipefail
exec bash ./caliban-adapter/install.sh
WRAPPER
cat > "$ADAPTER_DIR/start.sh" <<'WRAPPER'
#!/usr/bin/env bash
set -euo pipefail
exec bash ./caliban-adapter/run.sh > ./gateway_log.txt 2>&1
WRAPPER
cat > "$ADAPTER_DIR/settings.json" <<'WRAPPER'
{"graphql":"http://127.0.0.1:5220/graphql","health":"http://127.0.0.1:5220/health"}
WRAPPER
git -C "$SCRIPT_DIR" rev-parse --short HEAD > "$ADAPTER_DIR/version.txt"
chmod +x "$ADAPTER_DIR/install.sh" "$ADAPTER_DIR/start.sh"

echo "Prepared the Caliban adapter at $ADAPTER_DIR."
