#!/usr/bin/env sh

# source: https://misc.flogisoft.com/bash/tip_colors_and_formatting

# disabling warnings about unused variables
# shellcheck disable=SC2034

# reset all attributes
__COLOR_RESET='\e[0m'

# foreground colors
__COLOR_FG_DEFAULT='\e[39m'
__COLOR_FG_BLACK='\e[30m'
__COLOR_FG_RED='\e[31m'
__COLOR_FG_GREEN='\e[32m'
__COLOR_FG_YELLOW='\e[33m'
__COLOR_FG_BLUE='\e[34m'
__COLOR_FG_MAGENTA='\e[35m'
__COLOR_FG_CYAN='\e[36m'
__COLOR_FG_LIGHT_GRAY='\e[37m'

# background colors
__COLOR_BG_DEFAULT='\e[49m'
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
