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
__WINDOW_TITLE="\[\e]0;(${__USED_SHELL}) ${__LONG_PWD}\a\]"
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
PS1="${PS1}${__COLOR_FG_WHITE}${__BOLD_START}@${__COLOR_FG_BLUE}${__HOSTNAME}${__BOLD_END}${__COLOR_RESET}"
# current dir
PS1="${PS1}${__BOLD_START}:${__BOLD_END}"
PS1="${PS1}${__COLOR_FG_CYAN}${__BOLD_START}${__SHORT_PWD}${__BOLD_END}${__COLOR_RESET}"

# closing ]
PS1="${PS1}${__BOLD_START}]${__BOLD_END}"

# colored dollar sign depending on successful precommand
__COLOR_LAST_EXIT_0="${__COLOR_FG_YELLOW}"
__COLOR_LAST_EXIT_1="${__COLOR_FG_MAGENTA}"
__CHAR_LAST_EXIT_0='$'
__CHAR_LAST_EXIT_1='$'
if ( __is_zsh ); then
    # __COLOR_LAST_EXIT="%(?.${__COLOR_LAST_EXIT_0}.${__COLOR_LAST_EXIT_1})"

    __last_exit_char() {
        # shellcheck disable=2181
        # -> check ${?}, not cmd itself
        if [ "${?}" = 0 ]; then
            printf "%s%s%s%s%s" "${__COLOR_LAST_EXIT_0}" "${__BOLD_START}" "${__CHAR_LAST_EXIT_0}" "${__BOLD_END}" "${__COLOR_RESET}"
        else
            printf "%s%s%s%s%s" "${__COLOR_LAST_EXIT_1}" "${__BOLD_START}" "${__CHAR_LAST_EXIT_1}" "${__BOLD_END}" "${__COLOR_RESET}"
        fi
    }
elif ( __is_bash ); then
    __last_exit_char() {
        # shellcheck disable=2181
        # -> check ${?}, not cmd itself
        if [ "${?}" = 0 ]; then
            # shellcheck disable=2059
            # -> printf should interpret variable
            printf "${__COLOR_LAST_EXIT_0}${__BOLD_START}${__CHAR_LAST_EXIT_0}${__BOLD_END}${__COLOR_RESET}"
        else
            # shellcheck disable=2059
            # -> printf should interpret variable
            printf "${__COLOR_LAST_EXIT_1}${__BOLD_START}${__CHAR_LAST_EXIT_1}${__BOLD_END}${__COLOR_RESET}"
        fi
    }
fi
PS1="${PS1}\$(__last_exit_char) "

# if longer command -> begin next line with PS2
PS2='____$ '
