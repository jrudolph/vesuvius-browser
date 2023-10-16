#!/bin/sh

set -e
set -x

VERSION=$1

git tag -a $VERSION -m "Released $VERSION"
docker buildx build --builder kube --platform linux/arm64 --tag registry.virtual-void.net/jrudolph/vesuvius-browser:$VERSION --push .
