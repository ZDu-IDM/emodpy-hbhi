#!/bin/bash

# Shows job logs and detects success/failure.
# Inputs: log file
# Outputs: (log validation)
# Usage: ./monitor.sh

source common.sh

log_file=${1}

echo_step "Validate logs"
if [[ -s $log_file ]]; then
  [[ $(grep "failed to build" "${log_file}" -c) -gt 0 ]] && error "Build failed."
  [[ $(grep "couldn't find profile" "${log_file}" -c) -gt 0 ]] && error "Invalid Skaffold profile."
else
  error "${log_file} not found."
fi

echo
