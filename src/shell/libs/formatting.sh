#!/usr/bin/env sh

# disabling warnings about unused variables
# shellcheck disable=SC2034

. "${DOTFILES}/utils/faq.sh"

#------------------------------------------------------------------------------#
# formatting-settings

if ( __is_colored ); then
    # general formatting
    __COLOR_RESET='\001\e[0m\002'
    __BOLD_START='\001\e[1m\002'
    __BOLD_END='\001\e[0m\002'

    # foreground colors
    __COLOR_FG_BLACK='\001\e[30m\002'
    __COLOR_FG_RED='\001\e[31m\002'
    __COLOR_FG_GREEN='\001\e[32m\002'
    __COLOR_FG_YELLOW='\001\e[33m\002'
    __COLOR_FG_BLUE='\001\e[34m\002'
    __COLOR_FG_MAGENTA='\001\e[35m\002'
    __COLOR_FG_CYAN='\001\e[36m\002'
    __COLOR_FG_LIGHT_GRAY='\001\e[37m\002'

    # background colors
    __COLOR_BG_BLACK='\001\e[40m\002'
    __COLOR_BG_RED='\001\e[41m\002'
    __COLOR_BG_GREEN='\001\e[42m\002'
    __COLOR_BG_YELLOW='\001\e[43m\002'
    __COLOR_BG_BLUE='\001\e[44m\002'
    __COLOR_BG_MAGENTA='\001\e[45m\002'
    __COLOR_BG_CYAN='\001\e[46m\002'
    __COLOR_BG_LIGHT_GRAY='\001\e[47m\002'

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
