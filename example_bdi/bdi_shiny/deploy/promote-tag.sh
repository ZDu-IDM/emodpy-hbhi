#!/bin/bash

# Create promotion git tag and store used images in the message.
# This script is a part of the SAE images promotion process (see README).
# It is parsing images form the k8s deployment and constructs the git command with image names stored in the message.
# Inputs:
#   k8s namespace (containing sae deployment)
#   git tag to be created and the confirmation string
#   'Yes' as confirmation (Optional) - if not provided git command is only displayed
# Outputs: (git tag command string or created git tag in the repo - still requires manual tag push to origin)
# Usage: ./promote.sh sae-master v1.01 Yes

source common.sh

ns=$1
git_tag=$2
confirm=$3

image_base="malaria-bdi-hbhi"

[[ -z $ns ]] && error "Namespace must be specified as 1st argument."
[[ -z $git_tag ]] && error "GitHub tag must be specified as 2nd argument."

images=$(kubectl get pods -n $ns -o jsonpath="{.items[*].spec.containers[*].image}" | \
                            grep "${image_base}" | tr -s '[[:space:]]' | tr ' ' '\n' | uniq)
echo
echo "Found images:"
echo "${images}"

[[ -z $images ]] && error "Failed to retrieve images."

commit=$(cut -d ':' -f 2 <<<"${images%$'\n'*}" | cut -d '@' -f 1 | sed -E "s/-dirty//")
commit=${commit: -7}
echo
echo "Parsed commit:"
echo "${commit}"

read -r -d '' msg <<EOF
Promoted images
${images}
EOF

echo
echo_step "Constructed git command:"
echo "git tag -a $git_tag $commit -m \"${msg}\""

if [[ $confirm == "Yes" ]]; then
  printf "Setting git tag..."
  git tag -a $git_tag $commit -m "${msg}" || error "Setting tag failed."
  echo_ok
  git_msg="$(read_git_tag_msg ${git_tag})"
else
  git_msg="${msg}"
fi

read -r -d '' expected_images <<EOF
$(parse_image_from_tag "${git_msg}" "${image_base}" "${commit}")
EOF
echo
echo_step "Testing tag image parsing"
echo "Expected images:"
echo "${expected_images}"
echo
echo "Git tag message:"
echo "${git_msg}"
echo
printf "Verify parsed images..."
if [[ ${images} == "${expected_images}" ]]; then
  echo_ok
else
  error "not matching"
fi
