#!/usr/bin/env bash

if [[ -f "${DOTFILES}/custom/shell/func/is_machine" ]]; then
    . "${DOTFILES}/custom/shell/func/is_machine"
else
    . "${DOTFILES}/shell/func/is_machine"
fi

if ( is_machine 'ubuntu' ); then
    ############################################################################
    # install nord style for terminal

    mkdir -p "${DOTFILES}/custom/ubuntu"
    curl -so "${DOTFILES}/custom/ubuntu/nord.sh" https://raw.githubusercontent.com/arcticicestudio/nord-gnome-terminal/develop/src/nord.sh
    bash "${DOTFILES}/custom/ubuntu/nord.sh"
fi
