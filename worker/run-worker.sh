#!/bin/bash

set -ex

curl -JL -o /app.jar $APP_WORK_ENDPOINT/app
curl -o /deps.jar $APP_WORK_ENDPOINT/deps

java -cp /app.jar:/deps.jar net.virtualvoid.vesuvius.VesuviusWorkerMain