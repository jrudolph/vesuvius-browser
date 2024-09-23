ARG BASE_IMAGE_TAG
FROM --platform=$BUILDPLATFORM eclipse-temurin:17 AS builder

RUN apt-get update && \
    apt-get install --no-install-recommends -y git && \
    rm -rf /var/lib/apt/lists/*

RUN curl https://raw.githubusercontent.com/sbt/sbt/develop/sbt > /sbt && chmod +x /sbt && export PATH=/:$PATH

RUN mkdir -p /tmp/project/project
WORKDIR /tmp/project

# prime sbt for cache
COPY project/build.properties /tmp/project/project/
RUN /sbt exit

# warmup caches and create dependency jar, if build doesn't change, we will use caches in the next run
COPY build.sbt /tmp/project/
COPY project/* /tmp/project/project/

RUN /sbt -J-Xmx2g -J-verbose:gc update "show tiles/assemblyPackageDependency"

COPY .git /tmp/project/.git
RUN git reset --hard HEAD

RUN /sbt "show tiles/assembly"

FROM eclipse-temurin:17

RUN apt-get update && \
    apt-get install --no-install-recommends -y libvips-tools libtiff-tools && \
    rm -rf /var/lib/apt/lists/*

RUN curl -O https://repo1.maven.org/maven2/io/prometheus/jmx/jmx_prometheus_javaagent/0.16.1/jmx_prometheus_javaagent-0.16.1.jar

COPY --from=builder /tmp/project/tiles/target/deps.jar /deps.jar
COPY --from=builder /tmp/project/tiles/target/app.jar /app.jar
COPY jmx-config.yaml config.yaml

EXPOSE 8095/tcp

WORKDIR /

CMD ["java", "--add-opens", "java.base/sun.nio.ch=ALL-UNNAMED", "-javaagent:jmx_prometheus_javaagent-0.16.1.jar=9001:config.yaml", "-cp", "/app.jar:/deps.jar", "net.virtualvoid.vesuvius.tiles.TilesMain"]
