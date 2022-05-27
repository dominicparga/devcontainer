#!/usr/bin/env zsh

. "${DOTFILES}/src/shell/libs/faq.sh"

# COLORS

if ( is_colored ); then
    # general formatting
    __color_reset="%{${reset_color}%}"
    __bold_start='%B'
    __bold_end='%b'

    # foreground colors
    __color_fg_black="%{${fg[black]}%}"
    __color_fg_red="%{${fg[red]}%}"
    __color_fg_green="%{${fg[green]}%}"
    __color_fg_yellow="%{${fg[yellow]}%}"
    __color_fg_blue="%{${fg[blue]}%}"
    __color_fg_magenta="%{${fg[magenta]}%}"
    __color_fg_cyan="%{${fg[cyan]}%}"
    __color_fg_light_gray="%{${fg[white]}%}"

    # background colors
    __color_bg_black="%{${bg[black]}%}"
    __color_bg_red="%{${bg[red]}%}"
    __color_bg_green="%{${bg[green]}%}"
    __color_bg_yellow="%{${bg[yellow]}%}"
    __color_bg_blue="%{${bg[blue]}%}"
    __color_bg_magenta="%{${bg[magenta]}%}"
    __color_bg_cyan="%{${bg[cyan]}%}"
    __color_bg_light_gray="%{${bg[gray]}%}"

    # custom
    __color_info="${__color_fg_blue}"
    __color_warn="${__color_fg_yellow}"
    __color_err="${__color_fg_red}"
    __color_succ="${__color_fg_green}"
fi

# DETAILS

__used_shell='zsh'
__username='%n'
__hostname='%m'
__hostname_ip='%M'
__short_pwd='%2~'
__long_pwd='%~'
