#!/bin/sh

set -e
set -x

VERSION=$1

git tag -a worker-simple-$VERSION -m "Released worker $VERSION"
docker buildx build --builder kube --platform linux/amd64,linux/arm64 --tag registry.virtual-void.net/jrudolph/vesuvius-worker-simple:$VERSION --push -f worker/Dockerfile.simple .
