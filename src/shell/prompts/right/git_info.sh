#!/usr/bin/env zsh

. "${DOTFILES}/utils/faq.sh"

if ( __is_zsh ); then
    . "${DOTFILES}/src/shell/prompts/right/git_info.zsh"
fi
