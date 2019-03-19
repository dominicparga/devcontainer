# Following are set in shellrc.sh
# _custom_shell_lib
# _shell_lib

if [[ -n "${ZSH_NAME}" ]]; then
    ############################################################################
    # own functions

    fpath=(
        "${_custom_shell_lib}/func"
        "${_shell_lib}/func"
        "${fpath[@]}"
    )

    _dirs=( "${_shell_lib}" "${_custom_shell_lib}" )
    for _dir in "${_dirs[@]}"; do
        if [[ -d "${_dir}/func" ]]; then
            # is folder empty?
            if [[ -n "$(ls -A "${_dir}/func")" ]]; then
                for _file in "${_dir}/func/"*; do
                    autoload -Uz "${_file}";
                done
            fi
        fi
    done

    ############################################################################
    # brew autocompletion

    if [[ "$(which brew)" != 'brew not found' ]]; then
        # ATTENTION! has to be before 'autoload -Uz compinit'
        FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
    fi

    ############################################################################
    # others

    autoload -Uz compinit && compinit
    autoload colors && colors

elif [[ -n "${BASH}" ]]; then
    ############################################################################
    # own functions
    # check in both cases whether directory exists and is not empty

    _dirs=( "${_shell_lib}" "${_custom_shell_lib}" )
    for _dir in "${_dirs[@]}"; do
        echo "${_dir}"
        if [[ -d "${_dir}/func" ]]; then
            # is folder empty?
            if [[ -n "$(ls -A "${_dir}/func")" ]]; then
                for _file in "${_dir}/func/"*; do
                    . "${_file}";
                done
            fi
        fi
    done

    ############################################################################
    # brew autocompletion

    if [[ "$(which brew)" != 'brew not found' ]]; then
        for _file in "$(brew --prefix)/etc/bash_completion.d/"*; do
            [[ -f "${_file}" ]] && . "${_file}"
        done

        _file="$(brew --prefix)/etc/profile.d/bash_completion.sh"
        [[ -f "${_file}" ]] && . "${_file}"
    fi

    ############################################################################
    # (from previous, default bashrc on ubuntu)
    # enable programmable completion features (you don't need to enable this, if
    # it's already enabled in /etc/bash.bashrc and /etc/profile)

    if ! shopt -oq posix; then
        if [ -f '/usr/share/bash-completion/bash_completion' ]; then
            . '/usr/share/bash-completion/bash_completion'
        elif [ -f '/etc/bash_completion' ]; then
            . '/etc/bash_completion'
        fi
    fi
fi

unset _dir
unset _dirs
unset _file
