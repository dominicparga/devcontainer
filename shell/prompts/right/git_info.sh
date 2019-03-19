## Find my full .zshrc at <github.com/mislav/dotfiles/blob/master/zshrc>.

if [[ -n "${ZSH_NAME}" ]]; then

    ############################################################################
    # setup

    autoload colors && colors
    export LSCOLORS="Gxfxcxdxbxegedabagacad"
    setopt prompt_subst



    ############################################################################
    # prompt

    __ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[green]%}["
    __ZSH_THEME_GIT_PROMPT_SUFFIX="]%{$reset_color%}"
    __ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}*%{$reset_color%}"
    __ZSH_THEME_GIT_PROMPT_CLEAN=""

    # show git branch/tag, or name-rev if on detached head
    __parse_git_branch() {
        (command git symbolic-ref -q HEAD || command git name-rev --name-only --no-undefined --always HEAD) 2>/dev/null
    }

    # show red star if there are uncommitted changes
    __parse_git_dirty() {
        if command git diff-index --quiet HEAD 2> /dev/null; then
            echo "${__ZSH_THEME_GIT_PROMPT_CLEAN}"
        else
            echo "${__ZSH_THEME_GIT_PROMPT_DIRTY}"
        fi
    }

    # if in a git repo, show dirty indicator + git branch
    __git_custom_status() {
        local _git_where="$(__parse_git_branch)"
        [ -n "${_git_where}" ] && echo "$(__parse_git_dirty)${__ZSH_THEME_GIT_PROMPT_PREFIX}${_git_where#(refs/heads/|tags/)}${__ZSH_THEME_GIT_PROMPT_SUFFIX}"
    }

    # show current rbenv version if different from rbenv global
    __rbenv_version_status() {
        local _ver=$(rbenv version-name)
        [ "$(rbenv global)" != "${_ver}" ] && echo "[${_ver}]"
    }

    # put fancy stuff on the right
    if which rbenv &> /dev/null; then
        RPS1='$(__git_custom_status)%{$fg[red]%}$(__rbenv_version_status)%{$reset_color%} ${EPS1}'
    else
        RPS1='$(__git_custom_status) ${EPS1}'
    fi



elif [[ "${BASH}" ]]; then
    # unsupported yet
    :
fi
