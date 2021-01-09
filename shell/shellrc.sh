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

# general
export EDITOR='vim'
export VISUAL='code'

# git
if ( command -v "${VISUAL}" 1>/dev/null 2>&1); then
    export GIT_EDITOR="${VISUAL} --wait"
else
    export GIT_EDITOR="${EDITOR}"
fi

# python
# used in vscode to find a default python interpreter
export PYTHON_INTERPRETER_PATH='python'
export PIPENV_VENV_IN_PROJECT='yes'

# rust
export PATH="${HOME}/.cargo/bin:${PATH}"

#------------------------------------------------------------------------------#
# aliases

alias reboot='shutdown now --reboot'

alias cp='cp -i -P'
alias mv='mv -i'
alias grep='grep --color=auto'

alias c='clear'
# macOS: --color=auto needed for coreutils
alias l='ls -1F --color=auto'
alias la='l -a'
alias ll='l -lh'
alias lla='ll -a'

alias .2='cd ../..'
alias .3='cd ../../..'
alias .4='cd ../../../..'
alias .5='cd ../../../../..'
alias .6='cd ../../../../../..'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'

alias cddot="cd ${DOTFILES}"

# 'A' for ANSI line graphics
# 'C' for colorization
# 'F' for types, e.g. dir -> dir/
alias tree='tree -A -C -F'

alias g='git'

alias py='python'
alias py2='python2'
alias py3='python3'

#------------------------------------------------------------------------------#
# prompt

. "${__SHELL_LIB}/prompts/left/default.sh"
. "${__SHELL_LIB}/prompts/right/git_info.sh"

#------------------------------------------------------------------------------#
# cleanup

#unset DOTFILES
unset __SHELL_LIB
unset __CUSTOM_SHELL_LIB
