ARG BASE_IMAGE_TAG
FROM --platform=$BUILDPLATFORM eclipse-temurin:17 AS builder

RUN curl https://raw.githubusercontent.com/sbt/sbt/develop/sbt > /sbt && chmod +x /sbt && export PATH=/:$PATH

RUN mkdir -p /tmp/project/project
WORKDIR /tmp/project

# prime sbt for cache
COPY project/build.properties /tmp/project/project/
RUN /sbt exit

# warmup caches and create dependency jar, if build doesn't change, we will use caches in the next run
COPY build.sbt /tmp/project/
COPY project/* /tmp/project/project/

RUN /sbt -J-Xmx2g -J-verbose:gc update "show worker/assemblyPackageDependency"

COPY common /tmp/project/common
COPY worker/src /tmp/project/worker/src

RUN /sbt "show worker/assembly"

FROM registry2.virtual-void.net/jrudolph/vesuvius-worker-base:v1

RUN git clone -b changes https://github.com/jrudolph/Vesuvius-First-Letters /model

RUN cd /model && curl -OJL https://media.virtual-void.net/s/Pn7CFqPzpJJMJ4G/download/model.ckpt
RUN cd /model && pip3 install -r requirements.txt

RUN apt-get update && \
    apt-get install --no-install-recommends -y git curl openjdk-17-jre-headless libgl1 && \
    rm -rf /var/lib/apt/lists/*

COPY --from=builder /tmp/project/worker/target/scala-3.3.1/deps.jar /deps.jar
COPY --from=builder /tmp/project/worker/target/scala-3.3.1/app.jar /app.jar

WORKDIR /

CMD ["java", "-cp", "/app.jar:/deps.jar", "net.virtualvoid.vesuvius.VesuviusWorkerMain"]
