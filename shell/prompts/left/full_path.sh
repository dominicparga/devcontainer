################################################################################
# set variable identifying the chroot you work in (used in the prompt below)

if [ -z "${_debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    _debian_chroot=$(cat /etc/debian_chroot)
fi



################################################################################

if [[ -n "${ZSH_NAME}" ]]; then
    #autoload -Uz promptinit && promptinit && prompt default



    ############################################################################
    # colors

    # prefixes:
    # c - color
    local _c_default="%{${reset_color}%}"

    local _c_blue="%{$fg[blue]%}"
    local _c_cyan="%{$fg[cyan]%}"
    local _c_green="%{$fg[green]%}"
    local _c_red="%{$fg[red]%}"

    local _c_last_exit="%(?.${_c_green}.${_c_red})"



    ############################################################################
    # set prompt

    # reset
    PROMPT=""
    # window title
    case $TERM in
    xterm*)
        precmd () { print -Pn "\e]0;(zsh) %~\a"; }
        ;;
    esac
    # opening [
    PROMPT+="${_c_default}%B[%b"
    # debian root (if changed)
    PROMPT+="${_c_default}${_debian_chroot:+(${_debian_chroot})}"
    # $USERNAME
    PROMPT+="${_c_cyan}%B%n%b"
    # machine hostname
    PROMPT+="${_c_cyan}%B@%M%b"
    # current dir
    PROMPT+=":${_c_blue}%B%~%b"
    # closing ]
    PROMPT+="${_c_default}%B]%b"
    # colored dollar sign depending on successful precommand
    PROMPT+="${_c_last_exit}%B\$%b${_c_default} "

elif [[ -n "${BASH}" ]]; then
    PROMPT_COMMAND=__prompt_command

    __prompt_command() {
        local _last_exit_code="$?"
        local _cb_last_exit=""



        ########################################################################
        # colors

        # prefixes:
        # b - bold
        # c - color
        local _c_default="\[\e[0m\]"
        local _cb_default="\[\e[0;1m\]"

        local _cb_blue="\[\e[1;34m\]"
        local _cb_cyan="\[\e[1;36m\]"
        local _cb_green="\[\e[1;32m\]"
        local _cb_red="\[\e[1;31m\]"

        if [[ ${_last_exit_code} = 0 ]]; then
            _cb_last_exit="${_cb_green}"
        else
            _cb_last_exit="${_cb_red}"
        fi



        ########################################################################
        # set prompt

        # reset
        PS1=""
        # window title
        PS1+="\[\e]0;(bash) \w\a\]"
        # opening [
        PS1+="${_cb_default}["
        # debian root (if changed)
        PS1+="${_c_default}${_debian_chroot:+(${_debian_chroot})}"
        # $USERNAME
        PS1+="${_cb_cyan}\u"
        # machine hostname
        PS1+="${_cb_cyan}@\h"
        # current dir
        PS1+="${_cb_default}:${_cb_blue}\w"
        # closing ]
        PS1+="${_cb_default}]"
        # colored dollar sign depending on successful precommand
        PS1+="${_cb_last_exit}\$${_c_default} "
    }
fi
