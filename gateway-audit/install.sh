#!/bin/sh
set -eu

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <federation-gateway-audit-checkout>" >&2
    exit 1
fi

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
GATEWAY_DIR="$1/gateways/caliban"

(cd "$SCRIPT_DIR/.." && sbt gatewayAudit/assembly)
mkdir -p "$GATEWAY_DIR"
cp "$SCRIPT_DIR/gateway.json" "$SCRIPT_DIR/run.sh" "$GATEWAY_DIR/"
cp "$SCRIPT_DIR/target/caliban-gateway-audit.jar" "$GATEWAY_DIR/caliban.jar"
