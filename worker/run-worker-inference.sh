#!/bin/bash

set -ex

cd /model && \
  curl -OJL https://media.virtual-void.net/s/Pn7CFqPzpJJMJ4G/download/model.ckpt && \
  curl -o /model/grand-price-model.ckpt -JL https://media.virtual-void.net/s/iXFRiq8wRMHb9X7/download/timesformer_wild15_20230702185753_0_fr_i3depoch=12.ckpt

/run-worker.sh