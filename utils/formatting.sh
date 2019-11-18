#!/usr/bin/env sh

# disabling warnings about unused variables
# shellcheck disable=SC2034

. "${DOTFILES}/utils/faq.sh"

#------------------------------------------------------------------------------#
# formatting-settings

if ( __is_colored ); then
    # general formatting
    __COLOR_RESET='\e[0m'
    __BOLD_START='\e[1m'
    __BOLD_END='\e[0m'

    # foreground colors
    __COLOR_FG_BLACK='\e[30m'
    __COLOR_FG_RED='\e[31m'
    __COLOR_FG_GREEN='\e[32m'
    __COLOR_FG_YELLOW='\e[33m'
    __COLOR_FG_BLUE='\e[34m'
    __COLOR_FG_MAGENTA='\e[35m'
    __COLOR_FG_CYAN='\e[36m'
    __COLOR_FG_LIGHT_GRAY='\e[37m'

    # background colors
    __COLOR_BG_BLACK='\e[40m'
    __COLOR_BG_RED='\e[41m'
    __COLOR_BG_GREEN='\e[42m'
    __COLOR_BG_YELLOW='\e[43m'
    __COLOR_BG_BLUE='\e[44m'
    __COLOR_BG_MAGENTA='\e[45m'
    __COLOR_BG_CYAN='\e[46m'
    __COLOR_BG_LIGHT_GRAY='\e[47m'

    # custom
    __COLOR_INFO="${__COLOR_FG_BLUE}"
    __COLOR_WARN="${__COLOR_FG_YELLOW}"
    __COLOR_ERR="${__COLOR_FG_RED}"
    __COLOR_SUCC="${__COLOR_FG_GREEN}"
fi

#------------------------------------------------------------------------------#
# shell-info

__USED_SHELL='sh'
__USERNAME=''
__HOSTNAME=''
__HOSTNAME_IP=''
__SHORT_PWD=''
__LONG_PWD=''
