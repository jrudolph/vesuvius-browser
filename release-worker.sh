#!/bin/sh

set -e
set -x

VERSION=$1

git tag -a worker-$VERSION -m "Released worker $VERSION"
docker buildx build --builder kube --platform linux/amd64 --tag registry2.virtual-void.net/jrudolph/vesuvius-worker:$VERSION --push -f worker/Dockerfile .
