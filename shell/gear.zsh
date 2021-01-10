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
# autoloading others

autoload -Uz compinit && compinit
autoload colors && colors

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

#------------------------------------------------------------------------------#
# keybindings

# man zshzle
# shows manual for bindkey

# zle -al
# shows all available cmds

# showkey -a
# shows keys when pressed

# default: emacs
bindkey -e
# default: vi
#bindkey -v
# default: custom (minimal)
#bindkey -N custom .safe
#bindkey -A custom main

# - NAVIGATING / JUMPING -
# right
bindkey '^[[D' backward-char
# left
bindkey '^[[C' forward-char
# ctrl + right
bindkey '^[[1;5C' forward-word
# ctrl + left
bindkey '^[[1;5D' backward-word
# home
bindkey '^[[H' beginning-of-line
# end
bindkey '^[[F' end-of-line
# - DELETING -
# backspace
bindkey '^?' backward-delete-char
# delete
bindkey '^[[3~' delete-char
# ctrl + backspace
bindkey '^H' backward-delete-word
# ctrl + delete
bindkey '^[[3;5~' delete-word
# ctrl + home
bindkey '^[[1;5H' backward-kill-line
# ctrl + end
bindkey '^[[1;5F' kill-line
# - NAVIGATING HISTORY -
# ctrl + r
bindkey '^r' history-incremental-search-backward
# up
bindkey '^[[A' up-line-or-history
# down
bindkey '^[[B' down-line-or-history
