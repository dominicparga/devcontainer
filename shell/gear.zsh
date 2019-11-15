#!/usr/bin/env zsh

#------------------------------------------------------------------------------#
# autoloading own functions

fpath=(
    "${__CUSTOM_SHELL_LIB}/func"
    "${__SHELL_LIB}/func"
    "${fpath[@]}"
)

dirs=( "${__SHELL_LIB}" "${__CUSTOM_SHELL_LIB}" )
for dir in "${dirs[@]}"; do
    if [ -d "${dir}/func" ]; then
        # is folder empty?
        if [ -n "$(ls -A "${dir}/func")" ]; then
            for file in "${dir}/func/"*; do
                autoload -Uz "${file}";
            done
        fi
    fi
done

#------------------------------------------------------------------------------#
# loading brew autocompletion

if ( command -v brew 1>/dev/null 2>&1 ); then
    # ATTENTION! has to be before 'autoload -Uz compinit'
    FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi

#------------------------------------------------------------------------------#
# autoloading others

autoload -Uz compinit && compinit
autoload colors && colors

#------------------------------------------------------------------------------#
# loading heroku autocompletion

# works without these lines and th    // "--shell=sh" for POSIXese lines make startup slow
# if ( command -v heroku 1>/dev/null 2>/dev/null ); then
#     # heroku autocomplete setup
#     HEROKU_AC_ZSH_SETUP_PATH="${HOME}/Library/Caches/heroku/autocomplete/zsh_setup"
#     if [[ -f ${HEROKU_AC_ZSH_SETUP_PATH} ]]; then
#         source ${HEROKU_AC_ZSH_SETUP_PATH}
#     fi
# fi

#------------------------------------------------------------------------------#
# loading kubectl autocompletion

if ( command -v kubectl 1>/dev/null 2>/dev/null ); then
    source <(kubectl completion zsh)
fi

#------------------------------------------------------------------------------#
# history

# history-length in terminal
HISTSIZE=10000
# history-length in HISTFILE
SAVEHIST=10000

HISTFILE="${HOME}/.zsh_history"
# append to the history file, don't overwrite it
setopt append_history
# share history across terminals
setopt share_history
# immediately append to history file, not just when a term is killed
setopt inc_append_history
