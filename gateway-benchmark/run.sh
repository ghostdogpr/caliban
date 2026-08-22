#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
JAR="$SCRIPT_DIR/target/caliban-gateway-benchmark.jar"

if [ ! -f "$JAR" ]; then
    echo "The Caliban benchmark adapter is not built. Run ./install.sh first." >&2
    exit 1
fi

exec java ${JAVA_OPTS:-} -jar "$JAR"
