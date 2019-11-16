#!/usr/bin/env sh

# disabling warnings about unused variables
# shellcheck disable=SC2034

. "${DOTFILES}/utils/faq.sh"

#------------------------------------------------------------------------------#
# formatting-settings

if ( __is_colored ); then
    # general formatting
    __COLOR_RESET='\033[0m'
    __BOLD_START='\033[1m'
    __BOLD_END='\033[0m'

    # foreground colors
    __COLOR_FG_DEFAULT='\033[39m'
    __COLOR_FG_BLACK='\033[30m'
    __COLOR_FG_RED='\033[31m'
    __COLOR_FG_GREEN='\033[32m'
    __COLOR_FG_YELLOW='\033[33m'
    __COLOR_FG_BLUE='\033[34m'
    __COLOR_FG_MAGENTA='\033[35m'
    __COLOR_FG_CYAN='\033[36m'
    __COLOR_FG_LIGHT_GRAY='\033[37m'

    # background colors
    __COLOR_BG_DEFAULT='\033[49m'
    __COLOR_BG_BLACK='\033[40m'
    __COLOR_BG_RED='\033[41m'
    __COLOR_BG_GREEN='\033[42m'
    __COLOR_BG_YELLOW='\033[43m'
    __COLOR_BG_BLUE='\033[44m'
    __COLOR_BG_MAGENTA='\033[45m'
    __COLOR_BG_CYAN='\033[46m'
    __COLOR_BG_LIGHT_GRAY='\033[47m'

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
