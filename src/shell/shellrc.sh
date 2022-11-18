#!/usr/bin/env sh

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return ;;
esac

# INITIALIZATION

if [ -z "${DOTFILES}" ] || [ ! -d "${DOTFILES}" ]; then
    echo "ERROR: \${DOTFILES}=${DOTFILES}" >&2
    sleep 4
    exit 1
fi

. "${DOTFILES}/src/shell/libs/faq.sh"

# load interaction-defaults for used shell
if ( is_zsh ); then
    . "${DOTFILES}/src/shell/interaction-defaults.zsh"
elif ( is_bash ); then
    . "${DOTFILES}/src/shell/interaction-defaults.bash"
else
    echo "ERROR: Could not set interaction-defaults due to unknown shell."
fi

# EXPORTS

# general
export EDITOR='vim'
export VISUAL='code'
export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:'

# git
if ( command -v "${VISUAL}" 1>/dev/null 2>&1); then
    export GIT_EDITOR="${VISUAL} --wait"
else
    export GIT_EDITOR="${EDITOR}"
fi

# rust
export PATH="${HOME}/.cargo/bin:${PATH}"

# ALIASES

alias reboot='shutdown now --reboot'

alias cp='cp -i -P'
alias mv='mv -i'
alias grep='grep --color=auto'
alias diff='diff --color=auto'

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

# FROM MANJARO
# aliases
#alias ls="exa -alh"
#alias tree="exa --tree"
#alias cat="bat"
# use custom fd command for fzf incl. showing hidden files by default
#export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'

# PROMPT

. "${DOTFILES}/src/shell/prompts/left/default.sh"
. "${DOTFILES}/src/shell/prompts/right/git_info.sh"

# CLEANUP

#unset DOTFILES
unset SHELL_LIB
unset CUSTOM_SHELL_LIB
