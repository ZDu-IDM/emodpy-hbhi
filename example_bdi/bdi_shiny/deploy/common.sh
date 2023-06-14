#!/bin/bash

# Common functions.
# Inputs: none
# Outputs: none
# Usage:
#   source common.sh

function create_kaniko_namespace() {
  kaniko_ns="kaniko-$(hostname)" # alternative to below:  | tr '[:upper:]' '[:lower:]'
  kaniko_ns=${kaniko_ns,,}       # to lower case
  delete_kaniko_namespace

  echo "Creating kaniko build namespace ${kaniko_ns}"
  kubectl create ns $kaniko_ns
  kubectl config set-context --current --namespace=$kaniko_ns
  trap 'delete_kaniko_namespace' ERR SIGTERM INT TERM
}

function delete_kaniko_namespace() {
  echo "Deleting kaniko build namespace ${kaniko_ns} if it exists."
  kubectl delete ns $kaniko_ns &>/dev/null || echo "Skipping, namespace ${kaniko_ns} not found."
  kubectl config set-context --current --namespace=default
}

function echo_step() {
  echo
  echo "--=| $1 |=--"
  echo
}

GRN='\033[0;36m'
RED='\033[0;31m'
NC='\033[0m' # No Color

function echo_ok() {
  echo -e "[${GRN}OK${NC}] ${1}"
}

function error() {
  echo -e "${RED}ERROR:${NC} ${1}"
  exit 125
}

function parse_image_from_tag() {
  tag_msg=${1}
  image_name=${2}
  tag_commit=${3}
  echo "${tag_msg}" | grep "/${image_name}:.*${tag_commit}" # (@sha256:|)
}

function read_git_tag_msg() {
  target_tag=${1}
  read -r -d '' git_tag_msg <<EOF
$(git show "${target_tag}" -q)
EOF
  echo "${git_tag_msg}"
}
