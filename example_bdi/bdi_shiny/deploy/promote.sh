#!/bin/bash

# GitHub action promotes deployments to the Beta environment.
# This script is a part of the SAE images promotion process (see README).
# It is parsing images form the git tag message (set by promote-tag.sh) and populates the Beta deployment template.
# Depends on: promote-tag.sh
# Inputs: git tag
# Outputs: (image is promoted to Beta env.)
# Usage: ./promote.sh v1.01 Yes

source common.sh

git_tag=$1
confirm=$2
commit=$(git rev-list -n 1 $git_tag | cut -c-7)
image_base="malaria-bdi-hbhi"

[[ -z $git_tag ]] && error "GitHub tag must be specified as 1st argument."
[[ -z $commit ]] && error "Failed to parse the commit from tag ${git_tag}."

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)" && cd "${SCRIPT_DIR}"

source common.sh

git_msg="$(read_git_tag_msg ${git_tag})"
export image_full=$(parse_image_from_tag "${git_msg}" "${image_base}" "${commit}")
[[ -z $image_full ]] && error "Failed to retrieve image name"

[[ -f tags.json ]] && rm tags.json
image_name=$(echo $image_full | cut -d ':' -f 1)
echo "{\"builds\":[{\"imageName\":\"${image_name}\",\"tag\":\"${image_full}\"}]}" > tags.json
echo "Generated Skaffold tags.json"
cat tags.json
echo
[[ $confirm == "Yes" ]] && skaffold deploy -p prod --build-artifacts=tags.json