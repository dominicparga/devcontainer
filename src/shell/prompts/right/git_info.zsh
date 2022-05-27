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
    # -> doesn't get untracked files
    ! { command git diff-index --quiet HEAD 1>/dev/null 2>&1; }

    # gets untracked files due to '--other'
    # test $( git ls-files --modified --other --directory --exclude-standard | sed '1 q' | wc -l ) -gt 0
}

__update_RPS1() {
    #reset
    __rps1_commit_label=''

    # get commit-label and make it pretty
    if __rps1_commit_label="$(__get_commit_label)"; then
        # remove prefixes
        __rps1_commit_label="${__rps1_commit_label#(refs/heads/|tags/)}"

        # add brackets
        __rps1_commit_label="${__color_fg_cyan}[${__rps1_commit_label}]${__color_reset}"

        # add dirty-sign
        if ( __is_HEAD_dirty ); then
            __rps1_commit_label="${__color_fg_magenta}*${__color_reset}${__rps1_commit_label}"
        fi
    fi
    echo "${__rps1_commit_label}"
}

# setup
setopt prompt_subst
# prepare colors and details
. "${DOTFILES}/src/shell/libs/formatting.zsh"
# stick everything together
RPS1='$(__update_RPS1)'
