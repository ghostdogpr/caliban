#!/usr/bin/env bash

shopt -s globstar

for file in $2; do
  if grep -q "$1" "$file"; then
    echo "$1 exists in $file"
    exit 0
  else
    echo "$1 is missing in $file"
    exit 1
  fi
done
