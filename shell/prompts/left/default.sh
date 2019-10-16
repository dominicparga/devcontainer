is_color_prompt() {
    case "${TERM}" in
    xterm-color|*-256color)
        return 0
        ;;
    *)
        return 1
        ;;
    esac
}

prompt_cmd() {
    local _last_exit_code="${?}"

    #---------------------------------------------------------------------------
    # set variable identifying the chroot you work in (used in the prompt below)

    if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
        local debian_chroot=$(cat /etc/debian_chroot)
    fi

    #---------------------------------------------------------------------------
    # prepare colors and details

    if [[ -n "${ZSH_NAME}" ]]; then
        if ( is_color_prompt ); then
            # colors
            # b - bold
            # c - color
            local c_default="%{${reset_color}%}"
            local c_blue="%{${fg[blue]}%}"
            local c_cyan="%{${fg[cyan]}%}"
            local c_green="%{${fg[green]}%}"
            local c_magenta="%{${fg[magenta]}%}"
            local c_red="%{${fg[red]}%}"
            local c_yellow="%{${fg[yellow]}%}"
            local c_last_exit="%(?.${c_green}.${c_red})"
        fi

        local b_start='%B'
        local b_end='%b'

        # details
        local used_shell='zsh'
        local username='%n'
        local hostname='%m'
        local hostname_ip='%M'
        local short_pwd='%2~'
        local long_pwd='%~'

    elif [[ -n "${BASH}" ]]; then
        if ( is_color_prompt ); then
            # colors
            # b - bold
            # c - color
            local c_last_exit="${c_default}"

            local c_default='\[\e[0m\]'
            local c_blue='\[\e[0;34m\]'
            local c_cyan='\[\e[0;36m\]'
            local c_green='\[\e[0;32m\]'
            local c_magenta='\[\e[0;35m\]'
            local c_red='\[\e[0;31m\]'
            local c_yellow='\[\e[0;33m\]'

            local b_start='\[\e[1m\]'
            local b_end='\[\e[0m\]'

            # TODO (see also below with PROMPT_COMMAND)
            # Dynamic color of $ depending on $? works, but virtual-envs doesn't
            # due to reset of PS1.
            # if [[ ${_last_exit_code} = 0 ]]; then
            #     c_last_exit="${c_green}"
            # else
            #     c_last_exit="${c_red}"
            # fi
        fi

        # details
        local used_shell='bash'
        local username='\u'
        local hostname='\h'
        local hostname_ip='\H'
        local short_pwd='\W'
        local long_pwd='\w'
    fi

    #---------------------------------------------------------------------------
    # stick everything together

    # reset
    PS1=''
    # window title
    window_title="\[\e]0;(${used_shell}) ${long_pwd}\a\]"
    if [[ -n "${ZSH_NAME}" ]]; then
        case ${TERM} in
        xterm*)
            precmd() { print -Pn "${window_title}"; }
            ;;
        esac
    elif [[ -n "${BASH}" ]]; then
        PS1="${window_title}${PS1}"
    fi
    # opening [
    PS1+="${c_default}${b_start}[${b_end}"
    # debian root (if changed)
    PS1+="${c_default}${debian_chroot:+(${debian_chroot})}"
    # $USERNAME
    PS1+="${c_magenta}${b_start}${username}${b_end}"
    # hostname
    # PS1+="${c_magenta}${b_start}@${hostname}${b_end}"
    # current dir
    PS1+="${c_default}${b_start}:${b_end}"
    PS1+="${c_cyan}${b_start}${short_pwd}${b_end}"
    # closing ]
    PS1+="${c_default}${b_start}]${b_end}"
    # colored dollar sign depending on successful precommand
    PS1+="${c_last_exit}${b_start}\$${b_end}${c_default} "
}

if [[ -n "${ZSH_NAME}" ]]; then
    prompt_cmd
elif [[ -n "${BASH}" ]]; then
    # TODO (see also above with setting c_last_exit)
    # Dynamic color of $ depending on $? works, but virtual-envs doesn't
    # due to reset of PS1.
    # PROMPT_COMMAND=prompt_cmd
    prompt_cmd
fi

PS2='____$ '
