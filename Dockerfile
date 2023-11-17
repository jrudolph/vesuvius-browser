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

RUN /sbt -J-Xmx2g -J-verbose:gc update "show web/assemblyPackageDependency"

COPY .git /tmp/project/.git
RUN git reset --hard HEAD

RUN /sbt "show web/assembly"

FROM eclipse-temurin:17

RUN apt-get update && \
    apt-get install --no-install-recommends -y libvips-tools && \
    rm -rf /var/lib/apt/lists/*

RUN curl -O https://repo1.maven.org/maven2/io/prometheus/jmx/jmx_prometheus_javaagent/0.16.1/jmx_prometheus_javaagent-0.16.1.jar

COPY --from=builder /tmp/project/web/target/scala-3.3.1/deps.jar /deps.jar
COPY --from=builder /tmp/project/web/target/scala-3.3.1/app.jar /app.jar
COPY jmx-config.yaml config.yaml

EXPOSE 8089/tcp

WORKDIR /

CMD ["java", "-javaagent:jmx_prometheus_javaagent-0.16.1.jar=9001:config.yaml", "-XX:MaxRAMPercentage=60", "-cp", "/app.jar:/deps.jar", "net.virtualvoid.vesuvius.VesuviusWebMain"]
