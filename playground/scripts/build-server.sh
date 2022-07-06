#!/bin/bash

set -e

CURRENT_DIR=$(cd "$(dirname "$0")" && pwd)
PLAYGROUND_DIR="$CURRENT_DIR/.."
MYTE_ROOT_DIR="$PLAYGROUND_DIR/.."

docker build -t myte-playground -f "$PLAYGROUND_DIR/Dockerfile" "$MYTE_ROOT_DIR"
