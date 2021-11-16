#!/bin/bash

set -e
shopt -s extglob

CURRENT_DIR=$(cd "$(dirname "$0")" && pwd)

if [[ $# -ne 2 ]]; then
  echo "Usage: package_server.sh <MYTE_BIN> <STDLIB_DIR>";
  exit 1
fi

MYTE_BIN="$1"
STDLIB_DIR="$2"

# Create temp directory
WORKING_DIR=$(mktemp -d)

# Move all files for package into temp directory, starting with server files
cp "$CURRENT_DIR"/src/server/+(commands|lambda|runner).js "$WORKING_DIR"

# Include myte binary
cp "$MYTE_BIN" "$WORKING_DIR/myte"

# Include stdlib
cp -r "$STDLIB_DIR" "$WORKING_DIR/stdlib"

# Include server dependencies (system assembler and linker along with their shared libraries,
# as they are not included in base amazonlinux image)
cp -r "$CURRENT_DIR"/system-deps "$WORKING_DIR"

# Zip app files into server bundle
cd "$WORKING_DIR"
zip -r "$CURRENT_DIR/server.zip" ./*

# Clean up temp directory
rm -rf "$WORKING_DIR"