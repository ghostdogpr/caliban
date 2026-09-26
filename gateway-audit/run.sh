#!/bin/sh
set -eu

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <test-suite-id>" >&2
    exit 1
fi

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
JAR="$SCRIPT_DIR/target/caliban-gateway-audit.jar"

if [ ! -f "$JAR" ]; then
    echo "The Caliban audit adapter is not built. Run ./install.sh first." >&2
    exit 1
fi

exec java -jar "$JAR" "$1"
