#!/usr/bin/env sh

hub='docker.io/dominicparga'
image_name='devcontainer'
tag='2023050700'

docker_root_dirpath="$(dirname "$(readlink -f "${0}")")/../src"

docker \
    build \
    --file "${docker_root_dirpath}/Dockerfile" \
    --tag "${hub}/${image_name}:${tag}" \
    "${docker_root_dirpath}"

# docker push "${hub}/${image_name}:${tag}"
# docker push "${hub}/${image_name}:latest"
