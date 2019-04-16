# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return ;;
esac

################################################################################
# initialization

if [[ -z "${DOTFILES}" ]]; then
    export DOTFILES="${HOME}/dotfiles"
fi
_shell_lib="${DOTFILES}/shell"
_custom_shell_lib="${DOTFILES}/custom/shell"

################################################################################
# autoloading

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

    if ( which brew 1>/dev/null 2>/dev/null ); then
        # ATTENTION! has to be before 'autoload -Uz compinit'
        FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
    fi

    ############################################################################
    # others

    autoload -Uz compinit && compinit
    autoload colors && colors

    ############################################################################
    # heroku autocompletion

    # works without these lines and these lines make startup slow
    # if ( which heroku 1>/dev/null 2>/dev/null ); then
    #     # heroku autocomplete setup
    #     HEROKU_AC_ZSH_SETUP_PATH="${HOME}/Library/Caches/heroku/autocomplete/zsh_setup"
    #     if [[ -f ${HEROKU_AC_ZSH_SETUP_PATH} ]]; then
    #         . ${HEROKU_AC_ZSH_SETUP_PATH}
    #     fi
    # fi

    ############################################################################
    # kubectl autocompletion

    if ( which kubectl 1>/dev/null 2>/dev/null ); then
        . <(kubectl completion zsh)
    fi

elif [[ -n "${BASH}" ]]; then
    ############################################################################
    # own functions
    # check in both cases whether directory exists and is not empty

    _dirs=( "${_shell_lib}" "${_custom_shell_lib}" )
    for _dir in "${_dirs[@]}"; do
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

    if ( which brew 1>/dev/null 2>/dev/null ); then
        for _file in "$(brew --prefix)/etc/bash_completion.d/"*; do
            [[ -f "${_file}" ]] && . "${_file}"
        done

        _file="$(brew --prefix)/etc/profile.d/bash_completion.sh"
        [[ -f "${_file}" ]] && . "${_file}"
    fi

    ############################################################################
    # bash autocompletion

    if ( is_machine 'linux'); then
        # source /etc/bash.bashrc or /etc/profile ?
        if ! shopt -oq posix; then
            if [ -f '/usr/share/bash-completion/bash_completion' ]; then
                . '/usr/share/bash-completion/bash_completion'
            elif [ -f '/etc/bash_completion' ]; then
                . '/etc/bash_completion'
            fi
        fi
    elif ( is_machine 'macOS' ); then
        # bash-completion@2 installed via brew
        if [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]]; then
            . "/usr/local/etc/profile.d/bash_completion.sh"
        fi
    fi

    ############################################################################
    # heroku autocompletion

    if ( which heroku 1>/dev/null 2>/dev/null ); then
        # heroku autocomplete:script bash
        HEROKU_AC_BASH_SETUP_PATH="${HOME}/Library/Caches/heroku/autocomplete/bash_setup"
        if [[ -f ${HEROKU_AC_BASH_SETUP_PATH} ]]; then
            . ${HEROKU_AC_BASH_SETUP_PATH}
        fi
    fi

    ############################################################################
    # kubectl autocompletion

    if ( which kubectl 1>/dev/null 2>/dev/null ); then
        . <(kubectl completion bash)
    fi
fi

################################################################################
# history

HISTSIZE=10000
SAVEHIST=10000

if [[ -n "${ZSH_NAME}" ]]; then
    HISTFILE="${HOME}/.zsh_history"
    # append to the history file, don't overwrite it
    setopt append_history
    # share history across terminals
    setopt share_history
    # immediately append to history file, not just when a term is killed
    setopt inc_append_history

elif [[ -n "${BASH}" ]]; then
    HISTFILE="${HOME}/.bash_history"
    # append to the history file, don't overwrite it
    shopt -s histappend
fi

################################################################################
# exports

# coreutils
if ( is_machine 'macOS' ); then
    # head links to ghead instead of macOS-head
    PATH="/usr/local/opt/coreutils/libexec/gnubin:${PATH}"
fi

# general
export EDITOR='vi'
export VISUAL='code'

# git
if ( which "${VISUAL}" 1>/dev/null 2>/dev/null); then
    export GIT_EDITOR="${VISUAL} --wait"
else
    export GIT_EDITOR="${EDITOR}"
fi

# java
if ( is_machine 'macOS' ); then
    export JAVA_HOME="$(/usr/libexec/java_home)"
    export PATH="${JAVA_HOME}:${PATH}"
    export PATH="${JAVA_HOME}/bin:${PATH}"
fi

# nodejs and npm
if ( is_machine 'linux'); then
    VERSION='v11.10.0'
    DISTRO='linux-x64'
    export PATH="/usr/local/lib/nodejs/node-${VERSION}-${DISTRO}:${PATH}"
fi

# python
if ( is_machine 'linux' ); then
    # used in vscode to find a default python interpreter
    export PYTHON_INTERPRETER_PATH='' # TODO
elif ( is_machine 'macOS' ); then
    # path
    export PATH="/usr/local/opt/sqlite/bin:${PATH}"
    export PATH="/usr/local/opt/python/libexec/bin:${PATH}"
    # used in vscode to find a default python interpreter
    export PYTHON_INTERPRETER_PATH='/usr/local/opt/python3'
    # for pipenv
    export LC_ALL='en_US.UTF-8'
    export LANG='en_US.UTF-8'
fi
export PIPENV_VENV_IN_PROJECT='yes'

# rust
export PATH="${HOME}/.cargo/bin:${PATH}"

# nodejs and npm
if ( is_machine 'linux'); then
    VERSION='v11.10.0'
    DISTRO='linux-x64'
    export PATH="/usr/local/lib/nodejs/node-${VERSION}-${DISTRO}:${PATH}"
fi

# java
if ( is_machine 'macOS' ); then
    export JAVA_HOME="$(/usr/libexec/java_home)"
    export PATH="${JAVA_HOME}:${PATH}"
    export PATH="${JAVA_HOME}/bin:${PATH}"
fi

################################################################################
# aliases

alias cp='cp -i -P'
alias mv='mv -i'
alias grep='grep --color=auto'

alias c='clear'
# macOS: --color=auto needed for coreutils
alias l='ls -1GF --color=auto'
alias la='l -a'
alias ll='l -lh'
alias lla='ll -a'

alias .2='cd ../..'
alias ..='cd ..'
alias .3='cd ../../..'
alias ...='cd ../..'
alias .4='cd ../../../..'
alias ....='cd ../../../..'
alias .5='cd ../../../../..'
alias .....='cd ../../../../..'
alias .6='cd ../../../../../..'
alias ......='cd ../../../../../..'

alias cddot="cd ${DOTFILES}"

# 'A' for ANSI line graphics
# 'C' for colorization
alias tree='tree -AC'

alias g='git'

alias py='python'
alias py2='python2'
alias py3='python3'

if ( is_machine 'macOS' ); then
    alias javahome='/usr/libexec/java_home'
    alias javac8='javahome -v 1.8 --exec javac'
    alias jar8='javahome -v 1.8 --exec jar'
    alias java8='javahome -v 1.8 --exec java'
fi

################################################################################
# prompt

. "${_shell_lib}/prompts/left/lightweighted.sh"
. "${_shell_lib}/prompts/right/git_info.sh"

################################################################################
# cleanup

#unset DOTFILES
unset _shell_lib
unset _custom_shell_lib
unset _dir
unset _dirs
unset _file
