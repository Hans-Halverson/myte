#!/bin/bash

set -e

ECR_REPO=019582763512.dkr.ecr.us-east-1.amazonaws.com
DOCKER_IMAGE="$ECR_REPO/myte-playground:latest"

# Authenticate
aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin "$ECR_REPO"

# Publish docker image to ECR
docker tag myte-playground:latest "$DOCKER_IMAGE"
docker push "$DOCKER_IMAGE"

# Publish docker image to lambda
aws lambda update-function-code \
  --region us-east-1 \
  --function-name MytePlaygroundRunnerImage \
  --image-uri "$DOCKER_IMAGE"