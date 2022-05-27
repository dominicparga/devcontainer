#!/usr/bin/env zsh

. "${DOTFILES}/utils/faq.sh"

#------------------------------------------------------------------------------#
# colors

if ( __is_colored ); then
    # general formatting
    __COLOR_RESET="%{${reset_color}%}"
    __BOLD_START='%B'
    __BOLD_END='%b'

    # foreground colors
    __COLOR_FG_BLACK="%{${fg[black]}%}"
    __COLOR_FG_RED="%{${fg[red]}%}"
    __COLOR_FG_GREEN="%{${fg[green]}%}"
    __COLOR_FG_YELLOW="%{${fg[yellow]}%}"
    __COLOR_FG_BLUE="%{${fg[blue]}%}"
    __COLOR_FG_MAGENTA="%{${fg[magenta]}%}"
    __COLOR_FG_CYAN="%{${fg[cyan]}%}"
    __COLOR_FG_LIGHT_GRAY="%{${fg[white]}%}"

    # background colors
    __COLOR_BG_BLACK="%{${bg[black]}%}"
    __COLOR_BG_RED="%{${bg[red]}%}"
    __COLOR_BG_GREEN="%{${bg[green]}%}"
    __COLOR_BG_YELLOW="%{${bg[yellow]}%}"
    __COLOR_BG_BLUE="%{${bg[blue]}%}"
    __COLOR_BG_MAGENTA="%{${bg[magenta]}%}"
    __COLOR_BG_CYAN="%{${bg[cyan]}%}"
    __COLOR_BG_LIGHT_GRAY="%{${bg[gray]}%}"

    # custom
    __COLOR_INFO="${__COLOR_FG_BLUE}"
    __COLOR_WARN="${__COLOR_FG_YELLOW}"
    __COLOR_ERR="${__COLOR_FG_RED}"
    __COLOR_SUCC="${__COLOR_FG_GREEN}"
fi

#------------------------------------------------------------------------------#
# details

__USED_SHELL='zsh'
__USERNAME='%n'
__HOSTNAME='%m'
__HOSTNAME_IP='%M'
__SHORT_PWD='%2~'
__LONG_PWD='%~'
