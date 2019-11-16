#!/usr/bin/env zsh

. "${DOTFILES}/utils/faq.sh"

#------------------------------------------------------------------------------#
# colors

if ( __is_colored ); then
    __COLOR_RESET="%{${reset_color}%}"
    __COLOR_FG_BLUE="%{${fg[blue]}%}"
    __COLOR_FG_CYAN="%{${fg[cyan]}%}"
    __COLOR_FG_GREEN="%{${fg[green]}%}"
    __COLOR_FG_MAGENTA="%{${fg[magenta]}%}"
    __COLOR_FG_RED="%{${fg[red]}%}"
    __COLOR_FG_YELLOW="%{${fg[yellow]}%}"
fi

__BOLD_START='%B'
__BOLD_END='%b'

#------------------------------------------------------------------------------#
# details

__USED_SHELL='zsh'
__USERNAME='%n'
__HOSTNAME='%m'
__HOSTNAME_IP='%M'
__SHORT_PWD='%2~'
__LONG_PWD='%~'
