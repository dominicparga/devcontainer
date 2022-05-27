#!/usr/bin/env sh

. "${DOTFILES}/src/shell/libs/faq.sh"

if ( is_zsh ); then
    . "${DOTFILES}/src/shell/prompts/right/git_info.zsh"
fi
