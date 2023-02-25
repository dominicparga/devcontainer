#!/usr/bin/env sh

hub='docker.io/dominicparga'
image_name='devcontainer'
tag='2023022500'

docker \
    --context desktop-linux \
    run \
    --rm \
    --name devcontainer \
    --security-opt=no-new-privileges \
    "${hub}/${image_name}:${tag}"
