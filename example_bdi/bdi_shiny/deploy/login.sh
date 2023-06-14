#!/bin/bash

# Login to Azure set Azure subscription and set K8s context.
# Inputs: (Azure env. variables, see blow)
# Outputs: (authenticated with Azure and container registry, obtained K8s creds)
# Usage: ./login.sh

# Authentication, from GitHub runner secret
if [[ -n $AZ_SP_URL && -n $AZ_SP_PASSWORD && -n $AZ_TENANT ]]; then
  echo "Login to Azure as service principle (SP)"
  az login --service-principal -u ${AZ_SP_URL} -p ${AZ_SP_PASSWORD} --tenant ${AZ_TENANT}
else
  echo "Skipping login to Azure as SP. Environment variables AZ_SP_URL, AZ_SP_PASSWORD and AZ_TENANT are not set."
  az account show --query user.type &>/dev/null ||  { echo "Failed to login to Azure."; exit 125; }
fi

# Set Azure subscription
if [[ -n $AZ_SUB && -n $AZ_CLUSTER ]]; then
  az account set -s ${AZ_SUB}
  kubectl config use-context ${AZ_CLUSTER} || \
    az aks get-credentials --resource-group ${AZ_GROUP} --name ${AZ_CLUSTER}
else
  echo "Skipping k8s get-credentials. Environment variables AZ_SUB and AZ_CLUSTER are not set."
  kubectl cluster-info &>/dev/null ||  { echo "Failed to login to AKS."; exit 125; }
fi

# Login to the container registry
if [[ -n $AZ_CONTAINER_REGISTRY ]]; then
  token=$(az acr login --name ${AZ_CONTAINER_REGISTRY} --expose-token | jq -r '.accessToken')
  docker login ${AZ_CONTAINER_REGISTRY} -u 00000000-0000-0000-0000-000000000000 -p $token
else
  echo "Skipping login to container registry. Environment variable AZ_CONTAINER_REGISTRY is not set."
fi
