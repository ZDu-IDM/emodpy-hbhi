#!/bin/bash

# GitHub action executes to generate final manifests, build/push images and deploy to K8s.
# Inputs: profile name, (skaffold.yaml, login.sh, kustomize.sh)
# Outputs: (deployed app, logs)
# Usage: ./deploy.sh k8s/rseuxstage-pv2

source common.sh

profile=${1}
[[ -z $profile ]] && error "Please specify the skaffold profile."

echo "Deploying Skaffold profile '${profile}'"

echo_step "LOGIN & CONTEXT"
./login.sh

log_file=run.log
[[ -f $log_file ]] && rm $log_file
{
  echo_step "RUN"
  create_kaniko_namespace
  sleep 2
  #skaffold delete -p $profile
  skaffold run -p $profile
  delete_kaniko_namespace
} 2>&1 | tee $log_file
sleep 5

echo_step "MONITOR"
./verify-log.sh "${log_file}"
sleep 15
./verify-pods.sh malaria-apps bhi-hbhi
