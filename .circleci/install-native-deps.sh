#!/bin/bash
set -euo pipefail

# Scala Native warns that "versions older than clang 16 can contain known bugs
# and runtime issues". This image is based on Ubuntu 22.04, whose default
# `clang` package is 14, and that combination miscompiles the runtime: the
# native test binary intermittently dies with heap corruption ("malloc():
# corrupted top size", "Unhandled signal 11") or bogus MatchErrors.
# Pull a supported clang from apt.llvm.org and point Scala Native at it.
LLVM_VERSION=19

sudo apt-get update

sudo apt-get install -y \
  libstdc++-12-dev \
  libgc-dev \
  libuv1-dev \
  openssl \
  lsb-release \
  wget \
  gnupg \
  software-properties-common

wget -q https://apt.llvm.org/llvm.sh -O /tmp/llvm.sh
chmod +x /tmp/llvm.sh
sudo /tmp/llvm.sh "$LLVM_VERSION"

LLVM_BIN="/usr/lib/llvm-$LLVM_VERSION/bin"
sudo ln -sf "$LLVM_BIN/clang" /usr/local/bin/clang
sudo ln -sf "$LLVM_BIN/clang++" /usr/local/bin/clang++
# Scala Native reads LLVM_BIN to locate the toolchain; BASH_ENV carries it to
# the later steps of the job.
echo "export LLVM_BIN=$LLVM_BIN" >> "${BASH_ENV:-/dev/null}"

"$LLVM_BIN/clang" --version
