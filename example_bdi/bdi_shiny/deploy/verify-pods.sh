#!/bin/bash

# Runs by the prod monitoring GA to verify pods are up and they have the correct image.
# Inputs: k8s namespace (containing sae deployment), git commit/tag
# Outputs: (are pods and images OK)
# Usage:
# ./verify-pods.sh malaria-apps v1.01   # verify beta deployment for a specific tag
# ./verify-pods.sh malaria-apps         # verify the current master deployment for the most recent commit

source common.sh

ns=${1}
label=${2:-bhi-hbhi}

[[ -z $ns ]] && error "Please specify the namespace."
[[ -z $label ]] && error "Please specify the app label (e.g. ')."

commit_or_tag=${2:-$(git log -1 --format=%h)}           # the default is the latest commit from the current ref
commit=$(git rev-list -n 1 $commit_or_tag | cut -c-7)   # get commit of a tag (if commit is already set no change)

function validate_pods() {
  ns=$1
  label=$2

  printf "Checking %s pod count......" ${ns}
  pod_count=$(kubectl get pods -n "${ns}" -l "app=${label}" | grep Running -c)
  if [[ -n $pod_count && $pod_count -gt 0 ]]; then
    echo_ok "(found: ${pod_count})"
  else
    error "App ${label} pod count is 0."
  fi
}

echo_step "Validating deployments in namespace ${ns}"
validate_pods $ns $label
