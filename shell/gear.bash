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
