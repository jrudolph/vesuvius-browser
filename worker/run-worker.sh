#!/bin/bash

set -ex

curl --basic -u $APP_DATA_USERNAME:$APP_DATA_PASSWORD -o /app.jar $APP_WORK_ENDPOINT/app
curl --basic -u $APP_DATA_USERNAME:$APP_DATA_PASSWORD -o /deps.jar $APP_WORK_ENDPOINT/deps

java -cp /app.jar:/deps.jar net.virtualvoid.vesuvius.VesuviusWorkerMain