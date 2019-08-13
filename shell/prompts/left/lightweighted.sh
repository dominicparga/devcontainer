
__prompt_cmd() {
    ############################################################################
    # set variable identifying the chroot you work in (used in the prompt below)

    if [ -z "${_debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
        local _debian_chroot=$(cat /etc/debian_chroot)
    fi

    ############################################################################
    # prepare colors and details

    if [[ -n "${ZSH_NAME}" ]]; then
        # colors
        # b - bold
        # c - color
        local _c_default="%{${reset_color}%}"
        local _c_blue="%{${fg[blue]}%}"
        local _c_cyan="%{${fg[cyan]}%}"
        local _c_green="%{${fg[green]}%}"
        local _c_magenta="%{${fg[magenta]}%}"
        local _c_red="%{${fg[red]}%}"
        local _c_yellow="%{${fg[yellow]}%}"
        local _c_last_exit="%(?.${_c_green}.${_c_red})"

        local _b_start='%B'
        local _b_end='%b'

        # details
        local _used_shell='zsh'
        local _username='%n'
        local _hostname='%m'
        local _hostname_ip='%M'
        local _short_pwd='%2~'
        local _long_pwd='%~'

    elif [[ -n "${BASH}" ]]; then
        # colors
        # b - bold
        # c - color
        local _last_exit_code="${?}"
        local _c_last_exit=''

        local _c_default='\[\e[0m\]'
        local _c_blue='\[\e[0;34m\]'
        local _c_cyan='\[\e[0;36m\]'
        local _c_green='\[\e[0;32m\]'
        local _c_magenta='\[\e[0;35m\]'
        local _c_red='\[\e[0;31m\]'
        local _c_yellow='\[\e[0;33m\]'

        local _b_start='\[\e[1m\]'
        local _b_end='\[\e[0m\]'

        if [[ ${_last_exit_code} = 0 ]]; then
            _c_last_exit="${_c_green}"
        else
            _c_last_exit="${_c_red}"
        fi
        # since dynamic change is unsupported yet
        _c_last_exit="${_c_default}"

        # details
        local _used_shell='bash'
        local _username='\u'
        local _hostname='\h'
        local _hostname_ip='\H'
        local _short_pwd='\W'
        local _long_pwd='\w'
    fi

    # reset
    PS1=''
    # window title
    _window_title="\[\e]0;(${_used_shell}) ${_long_pwd}\a\]"
    if [[ -n "${ZSH_NAME}" ]]; then
        case ${TERM} in
        xterm*)
            precmd() { print -Pn "${_window_title}"; }
            ;;
        esac
    elif [[ -n "${BASH}" ]]; then
        PS1="${_window_title}${PS1}"
    fi
    # opening [
    PS1+="${_c_default}${_b_start}[${_b_end}"
    # debian root (if changed)
    PS1+="${_c_default}${_debian_chroot:+(${_debian_chroot})}"
    # $USERNAME
    PS1+="${_c_magenta}${_b_start}${_username}${_b_end}"
    # current dir
    PS1+="${_c_default}${_b_start}:${_b_end}"
    PS1+="${_c_cyan}${_b_start}${_short_pwd}${_b_end}"
    # closing ]
    PS1+="${_c_default}${_b_start}]${_b_end}"
    # colored dollar sign depending on successful precommand
    PS1+="${_c_last_exit}${_b_start}\$${_b_end}${_c_default} "
}

if [[ -n "${ZSH_NAME}" ]]; then
    __prompt_cmd
elif [[ -n "${BASH}" ]]; then
    # PROMPT_COMMAND=__prompt_cmd
    __prompt_cmd
fi

PS2="____$ "
