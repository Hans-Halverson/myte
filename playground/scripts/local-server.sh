#!/bin/bash

set -e

CURRENT_DIR=$(cd "$(dirname "$0")" && pwd)
PLAYGROUND_DIR="$CURRENT_DIR/.."

echo "Starting playground server docker container..."
CONTAINER_ID="$(docker run -d -p 9000:8080 myte-playground:latest)"

function cleanup {
  echo
  echo "Stopping playground server docker container..."
  docker stop "$CONTAINER_ID" > /dev/null
}

trap cleanup EXIT

echo "Starting local server"
node "$PLAYGROUND_DIR/src/local-server.js"
