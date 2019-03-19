################################################################################
# general

alias cd="cd -P"
alias cp="cp -i"
alias mv="mv -i"
alias grep="grep --color=auto"

alias c="clear"
# macOS: --color=auto needed for coreutils
alias l="ls -1GF --color=auto"
alias la="l -a"
alias ll="l -lh"
alias lla="ll -a"

alias .2="cd ../.."
alias ..="cd .."
alias .3="cd ../../.."
alias ...="cd ../.."
alias .4="cd ../../../.."
alias ....="cd ../../../.."
alias .5="cd ../../../../.."
alias .....="cd ../../../../.."
alias .6="cd ../../../../../.."
alias ......="cd ../../../../../.."

# 'A' for ANSI line graphics
# 'C' for colorization
alias tree="tree -AC"

################################################################################
# quick directory change

alias cddot="cd ${DOTFILES}"

################################################################################
# programming support

alias g=git

alias py='python'
alias py2='python2'
alias py3='python3'

if ( is_machine 'macOS' ); then
    alias javahome='/usr/libexec/java_home'
    alias javac8='javahome -v 1.8 --exec javac'
    alias jar8='javahome -v 1.8 --exec jar'
    alias java8='javahome -v 1.8 --exec java'
fi
