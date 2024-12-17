#!/bin/sh

set -e
set -x

VERSION=$1

docker buildx build --builder kube \
   --platform linux/arm64 \
   --build-arg BUILD_VERSION=$VERSION \
   --tag registry.virtual-void.net/jrudolph/vesuvius-browser:$VERSION \
   --push . && \
   git tag -a $VERSION -m "Released $VERSION"
