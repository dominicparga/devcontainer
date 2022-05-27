#!/usr/bin/env sh

is_zsh() {
    test -n "${ZSH_NAME}"
}

is_bash() {
    test -n "${BASH}"
}

is_machine() {
    machine=$(echo "${1}" | tr '[:upper:]' '[:lower:]')

    # -s is same as --kernel-name
    # but, of course, macOS doesn't support full flag o_O
    case "${machine}" in
    'macos'|'mac')
        uname -s | \
        grep --quiet --ignore-case --regexp='darwin.*';
        ;;
    'linux')
        uname -s | \
        grep --quiet --ignore-case --regexp='linux.*';
        ;;
    *)
        echo "unknown machine: ${1}"
        echo "current machine: $(uname -s)"
        false
        ;;
    esac
}

is_colored() {
    # https://unix.stackexchange.com/questions/198794/where-does-the-term-environment-variable-default-get-set
    if tput Co 1>/dev/null 2>&1; then
        if [ "$(tput Co)" -gt 2 ]; then
            return 0
        fi
    elif tput colors 1>/dev/null 2>&1; then
        if [ "$(tput colors)" -gt 2 ]; then
            return 0
        fi
    fi

    return 1
}
