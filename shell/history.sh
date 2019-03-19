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
