#!/bin/bash

# Runs by the prod monitoring GA to verify url is accessible.
# Inputs: none
# Outputs: (is url accessible)
# Usage:
# ./verify-url.sh

source common.sh

cnt=$(curl -kI --max-time 10 https://malaria-apps.idmod.org/malaria-bdi-hbhi/ | grep 'HTTP/1.1 200 OK' -wc)
if [[ $cnt == 1 ]]; then
  echo_ok "Site is accessible"
else
  error "Site is NOT accessible"
fi
