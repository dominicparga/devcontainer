#!/usr/bin/env zsh

__get_commit_label() {
    # either branch/tag or, if on detached head, name-rev
    # if no git-repository -> silent
    {
        if ! command git symbolic-ref --quiet HEAD; then
            command git name-rev --name-only --no-undefined --always HEAD
        fi
    } 2>/dev/null
}

__is_HEAD_dirty() {
    # diff-index returns true if no difference (like cmp returns equality)
    ! { command git diff-index --quiet HEAD 1>/dev/null 2>&1; }
}

__update_RPS1() {
    #reset
    __RPS1_COMMIT_LABEL=''

    # get commit-label and make it pretty
    if __RPS1_COMMIT_LABEL="$(__get_commit_label)"; then
        # remove prefixes
        __RPS1_COMMIT_LABEL="${__RPS1_COMMIT_LABEL#(refs/heads/|tags/)}"

        # add brackets
        __RPS1_COMMIT_LABEL="${__COLOR_FG_CYAN}[${__RPS1_COMMIT_LABEL}]${__COLOR_RESET}"

        # add dirty-sign
        if ( __is_HEAD_dirty ); then
            __RPS1_COMMIT_LABEL="${__COLOR_FG_MAGENTA}*${__COLOR_RESET}${__RPS1_COMMIT_LABEL}"
        fi
    fi
    echo "${__RPS1_COMMIT_LABEL}"
}

# setup
setopt prompt_subst
# prepare colors and details
. "${DOTFILES}/utils/formatting.zsh"
# stick everything together
RPS1='$(__update_RPS1)'
