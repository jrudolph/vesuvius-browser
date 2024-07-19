FROM registry2.virtual-void.net/jrudolph/vesuvius-worker-base:v1

RUN apt-get update && \
    apt-get install --no-install-recommends -y git curl openjdk-17-jre-headless libgl1 && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p /model

RUN git clone -b worker https://github.com/jrudolph/Vesuvius-First-Letters /model/first-letters
WORKDIR /model/first-letters
RUN git fetch origin && git checkout origin/worker && pip3 install --no-cache-dir -r requirements.txt

RUN git clone -b worker https://github.com/jrudolph/Vesuvius-Grandprize-Winner /model/grand-prize
WORKDIR /model/grand-prize
RUN git fetch origin && git checkout origin/worker && pip3 install --no-cache-dir -r requirements.txt

COPY worker/run-worker.sh /run-worker.sh
COPY worker/run-worker-inference.sh /run-worker-inference.sh

WORKDIR /

CMD ["/run-worker.sh"]
