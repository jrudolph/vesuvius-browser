#!/bin/sh

set -e
set -x

VERSION=$1

git tag -a tiles-$VERSION -m "Released tiles server $VERSION"
docker buildx build --builder kube --platform linux/arm64,linux/amd64 --tag registry.virtual-void.net/jrudolph/vesuvius-tiles:$VERSION --push -f tiles/Dockerfile .
