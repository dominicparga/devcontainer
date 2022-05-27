#!/usr/bin/env sh

# disabling warnings about unused variables
# shellcheck disable=SC2034

. "${DOTFILES}/src/shell/libs/faq.sh"

# FORMATTING-SETTINGS

if ( is_colored ); then
    # general formatting
    __color_reset='\001\e[0m\002'
    __bold_start='\001\e[1m\002'
    __bold_end='\001\e[0m\002'

    # foreground colors
    __color_fg_black='\001\e[30m\002'
    __color_fg_red='\001\e[31m\002'
    __color_fg_green='\001\e[32m\002'
    __color_fg_yellow='\001\e[33m\002'
    __color_fg_blue='\001\e[34m\002'
    __color_fg_magenta='\001\e[35m\002'
    __color_fg_cyan='\001\e[36m\002'
    __color_fg_light_gray='\001\e[37m\002'

    # background colors
    __color_bg_black='\001\e[40m\002'
    __color_bg_red='\001\e[41m\002'
    __color_bg_green='\001\e[42m\002'
    __color_bg_yellow='\001\e[43m\002'
    __color_bg_blue='\001\e[44m\002'
    __color_bg_magenta='\001\e[45m\002'
    __color_bg_cyan='\001\e[46m\002'
    __color_bg_light_gray='\001\e[47m\002'

    # custom
    __color_info="${__color_fg_blue}"
    __color_warn="${__color_fg_yellow}"
    __color_err="${__color_fg_red}"
    __color_succ="${__color_fg_green}"
fi

# SHELL-INFO

__used_shell='sh'
__username=''
__hostname=''
__hostname_ip=''
__short_pwd=''
__long_pwd=''
