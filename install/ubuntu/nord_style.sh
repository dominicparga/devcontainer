#!/usr/bin/env bash

################################################################################
# install nord style for terminal

mkdir -p "${DOTFILES}/custom/ubuntu"
curl -so "${DOTFILES}/custom/ubuntu/nord.sh" https://raw.githubusercontent.com/arcticicestudio/nord-gnome-terminal/develop/src/nord.sh
bash "${DOTFILES}/custom/ubuntu/nord.sh"
