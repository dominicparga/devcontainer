#!/usr/bin/env sh

# HELPERS

. "${DOTFILES}/src/shell/libs/faq.sh"

# PREPARE COLORS AND DETAILS

if ( is_zsh ); then
    . "${DOTFILES}/src/shell/libs/formatting.zsh"
elif ( is_bash ); then
    . "${DOTFILES}/src/shell/libs/formatting.bash"
fi

# STICK EVERYTHING TOGETHER
# goal:
# [dominic@aqua:~/dotfiles]$ echo 'yay my prompt'

# reset
PS1=''

# window title
# goal:
# (zsh) ~/dotfiles
__window_title="\[\e]0;(${__used_shell}) ${__long_pwd}\a\]"
if ( is_zsh ); then
    case ${TERM} in
    xterm*)
        precmd() { print -Pn "${__window_title}"; }
        ;;
    esac
elif ( is_bash ); then
    PS1="${__window_title}${PS1}"
fi

# opening [
PS1="${PS1}${__bold_start}[${__bold_end}"

# debian root (if changed)
# set variable identifying the chroot you work in (used in the prompt below)
__debian_chroot="${debian_chroot:-}"
if [ -z "${__debian_chroot}" ] && [ -r '/etc/debian_chroot' ]; then
    __debian_chroot=$(cat /etc/debian_chroot)
fi
__debian_chroot="${__debian_chroot:+(${__debian_chroot})}"
PS1="${PS1}${__debian_chroot}"

# username
PS1="${PS1}${__color_fg_magenta}${__bold_start}${__username}${__bold_end}${__color_reset}"

# hostname
PS1="${PS1}${__color_fg_white}${__bold_start}@${__color_fg_blue}${__hostname}${__bold_end}${__color_reset}"
# current dir
PS1="${PS1}${__bold_start}:${__bold_end}"
PS1="${PS1}${__color_fg_cyan}${__bold_start}${__short_pwd}${__bold_end}${__color_reset}"

# closing ]
PS1="${PS1}${__bold_start}]${__bold_end}"

# colored dollar sign depending on successful precommand
__color_last_exit_0="${__color_fg_yellow}"
__color_last_exit_1="${__color_fg_magenta}"
__char_last_exit_0='$'
__char_last_exit_1='$'
if ( is_zsh ); then
    # __color_last_exit="%(?.${__color_last_exit_0}.${__color_last_exit_1})"

    __last_exit_char() {
        # shellcheck disable=2181
        # -> check ${?}, not cmd itself
        if [ "${?}" = 0 ]; then
            printf "%s%s%s%s%s" "${__color_last_exit_0}" "${__bold_start}" "${__char_last_exit_0}" "${__bold_end}" "${__color_reset}"
        else
            printf "%s%s%s%s%s" "${__color_last_exit_1}" "${__bold_start}" "${__char_last_exit_1}" "${__bold_end}" "${__color_reset}"
        fi
    }
elif ( is_bash ); then
    __last_exit_char() {
        # shellcheck disable=2181
        # -> check ${?}, not cmd itself
        if [ "${?}" = 0 ]; then
            # shellcheck disable=2059
            # -> printf should interpret variable
            printf "${__color_last_exit_0}${__bold_start}${__char_last_exit_0}${__bold_end}${__color_reset}"
        else
            # shellcheck disable=2059
            # -> printf should interpret variable
            printf "${__color_last_exit_1}${__bold_start}${__char_last_exit_1}${__bold_end}${__color_reset}"
        fi
    }
fi
PS1="${PS1}\$(__last_exit_char) "

# if longer command -> begin next line with PS2
PS2='____$ '
