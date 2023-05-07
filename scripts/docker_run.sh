#!/usr/bin/env sh

hub='docker.io/dominicparga'
image_name='devcontainer'
tag='latest'

docker \
    run \
    --rm \
    --name devcontainer \
    --security-opt=no-new-privileges \
    "${hub}/${image_name}:${tag}"
