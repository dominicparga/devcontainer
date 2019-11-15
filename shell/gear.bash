#!/usr/bin/env bash

#------------------------------------------------------------------------------#
# sourcing own functions
# check in both cases whether directory exists and is not empty

__DIRS=( "${__SHELL_LIB}" "${__CUSTOM_SHELL_LIB}" )
for __DIR in "${__DIRS[@]}"; do
    if [ -d "${__DIR}/func" ]; then
        # is folder empty?
        if [ -n "$(ls -A "${__DIR}/func")" ]; then
            for __FILE in "${__DIR}/func/"*; do
                source "${__FILE}";
            done
        fi
    fi
done

#------------------------------------------------------------------------------#
# sourcing brew autocompletion

if ( command -v brew 1>/dev/null 2>&1 ); then
    for __FILE in "$(brew --prefix)/etc/bash_completion.d/"*; do
        [ -f "${__FILE}" ] && source "${__FILE}"
    done

    __FILE="$(brew --prefix)/etc/profile.d/bash_completion.sh"
    [ -f "${__FILE}" ] && source "${__FILE}"
fi

#------------------------------------------------------------------------------#
# sourcing bash autocompletion

if ( is_machine 'linux'); then
    # source /etc/bash.bashrc or /etc/profile ?
    if ! shopt -oq posix; then
        if [ -f '/usr/share/bash-completion/bash_completion' ]; then
            source '/usr/share/bash-completion/bash_completion'
        elif [ -f '/etc/bash_completion' ]; then
            source '/etc/bash_completion'
        fi
    fi
elif ( is_machine 'macOS' ); then
    # bash-completion@2 installed via brew
    if [ -r "/usr/local/etc/profile.d/bash_completion.sh" ]; then
        source "/usr/local/etc/profile.d/bash_completion.sh"
    fi
fi

#------------------------------------------------------------------------------#
# sourcing heroku autocompletion

if ( command -v heroku 1>/dev/null 2>&1 ); then
    # heroku autocomplete:script bash
    __HEROKU_AC_BASH_SETUP_PATH="${HOME}/Library/Caches/heroku/autocomplete/bash_setup"
    if [ -f "${__HEROKU_AC_BASH_SETUP_PATH}" ]; then
        source "${__HEROKU_AC_BASH_SETUP_PATH}"
    fi
fi

#------------------------------------------------------------------------------#
# sourcing kubectl autocompletion

if ( command -v kubectl 1>/dev/null 2>&1 ); then
    source <(kubectl completion bash)
fi

#------------------------------------------------------------------------------#
# history

# history-length in terminal
HISTSIZE=10000
# history-length in HISTFILE
HISTFILESIZE=10000

HISTFILE="${HOME}/.bash_history"
# append to the history file, don't overwrite it
shopt -s histappend
