#!/usr/bin/env bash

alias cp='cp -i -P'
alias mv='mv -i'
alias grep='grep --color=auto'
alias diff='diff --color=auto'

alias c='clear'
# macOS: --color=auto needed for coreutils
alias l='ls -1F --color=auto'
alias la='l -a'
alias ll='l -lFh'
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

# 'A' for ANSI line graphics
# 'C' for colorization
# 'F' for types, e.g. dir -> dir/
alias tree='tree -A -C -F'

alias g='git'

alias py='python'
alias py3='python3'