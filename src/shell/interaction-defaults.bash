#!/usr/bin/env bash

#------------------------------------------------------------------------------#
# sourcing own functions
# check in both cases whether directory exists and is not empty

dirs=( "${DOTFILES}/src/shell" "${DOTFILES}/custom/shell" )
for dir in "${dirs[@]}"; do
    if [ -d "${dir}/func" ]; then
        # is folder empty?
        if [ -n "$(ls -A "${dir}/func")" ]; then
            for FILE in "${dir}/func/"*; do
                source "${FILE}";
            done
        fi
    fi
done

#------------------------------------------------------------------------------#
# sourcing bash autocompletion

# source /etc/bash.bashrc or /etc/profile ?
if ! shopt -oq posix; then
    if [ -r '/usr/share/bash-completion/bash_completion' ]; then
        source '/usr/share/bash-completion/bash_completion'
    elif [ -r '/etc/bash_completion' ]; then
        source '/etc/bash_completion'
    fi
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

#------------------------------------------------------------------------------#
# keybindings

# man bind
# does not show the right function.
# Google for it, e.g. https://www.computerhope.com/unix/bash/bind.htm

# bind -l
# shows all available cmds

# showkey -a
# shows keys when pressed

# default: emacs
bind -m emacs
#set -o emacs
# default: vi
#set -o vi

# - NAVIGATING / JUMPING -
# right
bind '"\e[D": backward-char'
# left
bind '"\e[C": forward-char'
# ctrl + right
bind '"\e[1;5C": forward-word'
# ctrl + left
bind '"\e[1;5D": backward-word'
# home
bind '"\e[H": beginning-of-line'
# end
bind '"\e[F": end-of-line'
# - DELETING -
# backspace
bind '"^?": backward-delete-char'
# delete
bind '"\e[3~": delete-char'
# ctrl + backspace
bind '"^H": backward-delete-word'
# ctrl + delete
bind '"\e[3;5~": delete-word'
# ctrl + home
bind '"\e[1;5H": backward-kill-line'
# ctrl + end
bind '"\e[1;5F": kill-line'
# - NAVIGATING HISTORY -
# ctrl + r
bind '"^r": history-incremental-search-backward'
# up
bind '"\e[A": previous-history'
# down
bind '"\e[B": next-history'
