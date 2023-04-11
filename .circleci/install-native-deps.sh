#!/bin/bash

sudo apt-get update

sudo apt-get install --no-install-recommends -y \
  clang \
  libstdc++-12-dev \
  libgc-dev \
  openssl
