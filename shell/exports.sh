################################################################################
# coreutils

if ( is_machine 'macOS' ); then
    # head links to ghead instead of macOS-head
    PATH="/usr/local/opt/coreutils/libexec/gnubin:${PATH}"
fi

################################################################################
# general

export EDITOR='vi'
export VISUAL='code'

################################################################################
# git

if [[ "$(which ${VISUAL})" != "${VISUAL} not found" ]]; then
    export GIT_EDITOR="${VISUAL} --wait"
else
    export GIT_EDITOR="${EDITOR}"
fi

################################################################################
# python

if ( is_machine 'macOS' ); then
    # used in vscode to find a default python interpreter
    export PYTHON_INTERPRETER_PATH='/usr/local/bin/python3'

    export PATH="${HOME}/Library/Python/2.7/bin:${PATH}"
    export PATH="${HOME}/Library/Python/3.7/bin:${PATH}"
fi

################################################################################
# java

if ( is_machine 'macOS' ); then
    export JAVA_HOME="$(/usr/libexec/java_home)"
    export PATH="${JAVA_HOME}:${PATH}"
    export PATH="${JAVA_HOME}/bin:${PATH}"
fi
