# FROM docker.io/ubuntu
# FROM mcr.microsoft.com/devcontainers/base:latest
FROM mcr.microsoft.com/devcontainers/base:ubuntu
RUN apt-get update &&\
    apt-get install --yes --no-install-suggests --no-install-recommends \
        ca-certificates \
        git \
        less \
        openssh-client \
        vim \
    &&\
    apt-get clean

ARG USERNAME=vscode

# USER root
#
# Create the user
# EDIT: Nope because already exists
# RUN useradd --system --create-home --shell /bin/bash ${USERNAME}
    #
    # [Optional] Add sudo support. Omit if you don't need to install software after connecting.
    # && apt-get update \
    # && apt-get install -y sudo \
    # && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    # && chmod 0440 /etc/sudoers.d/$USERNAME

USER ${USERNAME}

# git
WORKDIR /home/${USERNAME}
COPY home/* .

# credits 8)
LABEL maintainer="dominicparga"
LABEL description="My base VSCode .devcontainer image"
