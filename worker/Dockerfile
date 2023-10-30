FROM registry2.virtual-void.net/jrudolph/vesuvius-worker-base:v1

RUN git clone -b changes https://github.com/jrudolph/Vesuvius-First-Letters /model

RUN cd /model && curl -OJL https://media.virtual-void.net/s/Pn7CFqPzpJJMJ4G/download/model.ckpt
RUN cd /model && pip3 install -r requirements.txt

RUN apt-get update && \
    apt-get install --no-install-recommends -y git curl openjdk-17-jre-headless libgl1 && \
    rm -rf /var/lib/apt/lists/*

COPY worker/run-worker.sh /run-worker.sh

WORKDIR /

CMD ["./run-worker.sh"]
