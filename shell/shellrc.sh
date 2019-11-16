#!/usr/bin/env sh

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return ;;
esac

#------------------------------------------------------------------------------#
# initialization

if [ -z "${DOTFILES}" ] || [ ! -d "${DOTFILES}" ]; then
    echo "ERROR: \${DOTFILES} is set incorrectly or is empty: ${DOTFILES}" >&2
    sleep 4
    exit 1
fi
export __SHELL_LIB="${DOTFILES}/shell"
export __CUSTOM_SHELL_LIB="${DOTFILES}/custom/shell"

. "${DOTFILES}/utils/faq.sh"

#------------------------------------------------------------------------------#
# load gear for used shell

if ( __is_zsh ); then
    . "${DOTFILES}/shell/gear.zsh"
elif ( __is_bash ); then
    . "${DOTFILES}/shell/gear.bash"
fi

#------------------------------------------------------------------------------#
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
if ( command -v "${VISUAL}" 1>/dev/null 2>&1); then
    export GIT_EDITOR="${VISUAL} --wait"
else
    export GIT_EDITOR="${EDITOR}"
fi

# java
if ( is_machine 'macOS' ); then
    JAVA_HOME="$(/usr/libexec/java_home)"
    export JAVA_HOME
    export PATH="${JAVA_HOME}:${PATH}"
    export PATH="${JAVA_HOME}/bin:${PATH}"
fi

# nodejs and npm
if ( is_machine 'linux'); then
    _npm_version='v11.10.0'
    _npm_distro='linux-x64'
    export PATH="/usr/local/lib/nodejs/node-${_npm_version}-${_npm_distro}/bin:${PATH}"
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

# compiler-needs since brew doesn't replace macOS bins
if ( is_machine 'macOS' ); then
    # icu4c
    # export PATH="/usr/local/opt/icu4c/bin:${PATH}"
    # export PATH="/usr/local/opt/icu4c/sbin:${PATH}"
    export LDFLAGS="-L/usr/local/opt/icu4c/lib"
    export CPPFLAGS="-I/usr/local/opt/icu4c/include"
    # sqliteSC2139"
    # readline
    export LDFLAGS="-L/usr/local/opt/readline/lib"
    export CPPFLAGS="-I/usr/local/opt/readline/include"
fi

#------------------------------------------------------------------------------#
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

#------------------------------------------------------------------------------#
# prompt

. "${__SHELL_LIB}/prompts/left/default.sh"
. "${__SHELL_LIB}/prompts/right/git_info.sh"

#------------------------------------------------------------------------------#
# cleanup

#unset DOTFILES
unset __SHELL_LIB
unset __CUSTOM_SHELL_LIB
