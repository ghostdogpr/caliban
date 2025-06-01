#!/usr/bin/env bash

if grep -q "$1" "$2"; then
  echo "$1 exists in $2"
  exit 0
else
  echo "$1 is missing in $2"
  exit 1
fi