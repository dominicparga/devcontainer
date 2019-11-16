#!/usr/bin/env sh

#------------------------------------------------------------------------------#
# helpers

. "${DOTFILES}/utils/faq.sh"

#------------------------------------------------------------------------------#
# prepare colors and details

if ( __is_zsh ); then
    . "${DOTFILES}/utils/formatting.zsh"
elif ( __is_bash ); then
    . "${DOTFILES}/utils/formatting.bash"
fi

#---------------------------------------------------------------------------
# stick everything together
# goal:
# [dominic:~/dotfiles]$ echo 'yay my prompt'

# reset
PS1=''

# window title
# goal:
# (zsh) ~/dotfiles
__WINDOW_TITLE="\033]0;(${__USED_SHELL}) ${__LONG_PWD}\a\]"
if ( __is_zsh ); then
    case ${TERM} in
    xterm*)
        precmd() { print -Pn "${__WINDOW_TITLE}"; }
        ;;
    esac
elif ( __is_bash ); then
    PS1="${__WINDOW_TITLE}${PS1}"
fi

# opening [
PS1="${PS1}${__BOLD_START}[${__BOLD_END}"

# debian root (if changed)
# set variable identifying the chroot you work in (used in the prompt below)
__DEBIAN_CHROOT="${debian_chroot:-}"
if [ -z "${__DEBIAN_CHROOT}" ] && [ -r '/etc/debian_chroot' ]; then
    __DEBIAN_CHROOT=$(cat /etc/debian_chroot)
fi
__DEBIAN_CHROOT="${__DEBIAN_CHROOT:+(${__DEBIAN_CHROOT})}"
PS1="${PS1}${__DEBIAN_CHROOT}"

# username
PS1="${PS1}${__COLOR_FG_MAGENTA}${__BOLD_START}${__USERNAME}${__BOLD_END}${__COLOR_RESET}"

# hostname
# PS1="${PS1}${__COLOR_FG_MAGENTA}${__BOLD_START}@${__HOSTNAME}${__BOLD_END}${__COLOR_RESET}"
# current dir
PS1="${PS1}${__BOLD_START}:${__BOLD_END}"
PS1="${PS1}${__COLOR_FG_CYAN}${__BOLD_START}${__SHORT_PWD}${__BOLD_END}${__COLOR_RESET}"

# closing ]
PS1="${PS1}${__BOLD_START}]${__BOLD_END}"

# colored dollar sign depending on successful precommand
__COLOR_LAST_EXIT_0="${__COLOR_FG_YELLOW}"
__COLOR_LAST_EXIT_1="${__COLOR_FG_MAGENTA}"
__LAST_EXIT_CHAR='#'
if ( __is_zsh ); then
    __COLOR_LAST_EXIT="%(?.${__COLOR_LAST_EXIT_0}.${__COLOR_LAST_EXIT_1})"
    PS1="${PS1}${__COLOR_LAST_EXIT}${__BOLD_START}${__LAST_EXIT_CHAR}${__BOLD_END}${__COLOR_RESET} "
elif ( __is_bash ); then
    __color_last_exit() {
        # shellcheck disable=2181
        # -> check ${?}, not cmd itself
        if [ "${?}" = 0 ]; then
            # shellcheck disable=2059
            # -> printf should interpret variable
            printf "${__COLOR_LAST_EXIT_0}"
        else
            # shellcheck disable=2059
            # -> printf should interpret variable
            printf "${__COLOR_LAST_EXIT_1}"
        fi
    }
    PS1="${PS1}\$(__color_last_exit)${__BOLD_START}${__LAST_EXIT_CHAR}${__BOLD_END}${__COLOR_RESET} "
fi

# if longer command -> begin next line with PS2
PS2='____$ '
