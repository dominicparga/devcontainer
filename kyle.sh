#!/usr/bin/env sh

# DEFAULTS

F_CREATE='create'
F_SYMLINK='symlink'
F_P_ALL='all'
F_P_SHELL='shell'
F_P_GIT='git'
F_P_VSCODE='vscode'
F_P_ALACRITTY='alacritty'
F_P_TERMUX='termux'
F_P_R='R'

ARG_F_P="F_${F_CREATE}_P_${F_P_ALL}"
ARG_IS_FORCED=false

exists() {
    [ -e "${1}" ] || [ -L "${1}" ]
}

yn_is_yes() {
    case "${yn}" in
    'y'|'Y'|'yes'|'YES') true;;
    *) false;;
    esac
}

safe_rm() {
    new_file="${1}"
    if "${ARG_IS_FORCED}"; then
        rm -f "${new_file}"
    elif exists "${new_file}"; then
        if [ -L "${new_file}" ]; then
            printf "Replace %s@? [y|n] " "${new_file}"
        else
            printf "Replace %s? [y|n] " "${new_file}"
        fi
        read -r yn
        yn_is_yes && rm "${new_file}"
    fi
}

safe_ln() {
    src="${1}"
    dst="${2}"
    safe_rm "${src}"
    if ! exists "${src}"; then
        mkdir -p "$(dirname "${src}")"
        ln -s "${dst}" "${src}"
        # If a new file was created but 'no' has been selected in prompt,
        # the file is brand new, hence echo message for user.
        if ! yn_is_yes; then
            echo "INFO: Created ${src}@"
        fi
    fi
}

safe_cp() {
    src="${1}"
    dst="${2}"
    safe_rm "${dst}"
    if ! exists "${dst}"; then
        mkdir -p "$(dirname "${dst}")"
        cp --no-dereference "${src}" "${dst}"
        # If a new file was created but 'no' has been selected in prompt,
        # the file is brand new, hence echo message for user.
        if ! yn_is_yes; then
            echo "INFO: Created ${dst}"
        fi
    fi
}

symlink_r() {
    safe_ln "${HOME}/.Rprofile" "${DOTFILES}/custom/R/profile.R"
    safe_ln "${HOME}/.Renviron" "${DOTFILES}/custom/R/environ.sh"
}

create_r() {
    symlink_r

    safe_ln "${DOTFILES}/custom/R/profile.R" "${DOTFILES}/src/R/profile.R"
    safe_ln "${DOTFILES}/custom/R/environ.sh" "${DOTFILES}/src/R/environ.sh"
}

symlink_termux() {
    safe_ln "${HOME}/.termux/termux.properties" "${DOTFILES}/custom/termux.properties"
}

create_termux() {
    symlink_termux

    safe_ln "${DOTFILES}/custom/termux.properties" "${DOTFILES}/src/termux.properties"
}

symlink_alacritty() {
    safe_ln "${HOME}/.alacritty.yml" "${DOTFILES}/custom/alacritty.yml"
}

create_alacritty() {
    symlink_alacritty

    safe_ln "${DOTFILES}/custom/alacritty.yml" "${DOTFILES}/src/alacritty.yml"
}

set_vscode_home() {
    # VSCODE_HOME depends on OS, see
    # https://code.visualstudio.com/docs/getstarted/settings#_settings-file-locations
    #
    # macOS
    # linux - microsoft-release
    # linux - open-source-release
    # windows (path is for git-bash)
    for dir in \
        "${HOME}/Library/Application Support/Code/User" \
        "${HOME}/.config/Code/User" \
        "${HOME}/.config/Code - OSS/User" \
        "${HOME}/AppData/Roaming/Code/User"
    do
        unset VSCODE_HOME
        if exists "${dir}"; then
            VSCODE_HOME="${dir}"
            break
        fi
    done

    if [ -z "${VSCODE_HOME}" ]; then
        echo "ERROR: Could not find vscode-folder"
        false
    else
        true
    fi
}

symlink_vscode() {
    if set_vscode_home; then
        safe_ln "${VSCODE_HOME}/settings.json" "${DOTFILES}/custom/vscode/settings.json"
        safe_ln "${VSCODE_HOME}/keybindings.json" "${DOTFILES}/custom/vscode/keybindings.json"
    fi
}

create_vscode() {
    symlink_vscode

    if set_vscode_home; then
        safe_ln "${DOTFILES}/custom/vscode/settings.json" "${DOTFILES}/src/vscode/settings.json"
        safe_ln "${DOTFILES}/custom/vscode/keybindings.json" "${DOTFILES}/src/vscode/keybindings.json"
    fi
}

symlink_git() {
    safe_ln "${HOME}/.gitconfig" "${DOTFILES}/custom/git/config"
    safe_ln "${HOME}/.gitconfig.global" "${DOTFILES}/custom/git/config.global"
}

create_git() {
    symlink_git

    # remember for later
    if ! exists "${DOTFILES}/custom/git/config"; then
        is_custom_git_config_missing=true
    else
        is_custom_git_config_missing=false
    fi
    safe_cp "${DOTFILES}/src/git/config" "${DOTFILES}/custom/git/config"
    # if custom git-config has not been existed
    # or if it has but was removed
    if "${is_custom_git_config_missing}" || yn_is_yes; then
        echo "WARN: Update your user in ${DOTFILES}/custom/git/config."
    fi
    safe_ln "${DOTFILES}/custom/git/config.global" "${DOTFILES}/src/git/config.global"
}

symlink_ssh() {
    safe_ln "${HOME}/.ssh/config" "${DOTFILES}/custom/shell/ssh/config"
}

create_ssh() {
    symlink_ssh

    safe_cp "${DOTFILES}/src/shell/ssh/config" "${DOTFILES}/custom/shell/ssh/config"
}

symlink_shell() {
    safe_ln "${HOME}/.zshrc" "${HOME}/.profile"
    safe_ln "${HOME}/.bashrc" "${HOME}/.profile"
    safe_ln "${HOME}/.profile" "${DOTFILES}/custom/shell/shellrc.sh"
}

create_shell() {
    symlink_shell

    dst="${DOTFILES}/custom/shell/shellrc.sh"
    safe_rm "${dst}"
    if ! exists "${dst}"; then
        mkdir -p "$(dirname "${dst}")"
        mkdir -p "$(dirname "${dst}")/func"
        touch "${dst}"
        {
            echo '#!/usr/bin/env sh'
            echo ''
            echo "# DO NOTHING IF NOT RUNNING INTERACTIVELY"
            echo ''
            echo 'case $- in'
            echo '    *i*) ;;'
            echo '      *) return ;;'
            echo 'esac'
            echo ''
            echo '# SETUP'
            echo ''
            echo "export DOTFILES=\"${DOTFILES}\""
            echo '. "${DOTFILES}/src/shell/shellrc.sh"'
            echo ''
            echo '# EXPORTS'
            echo ''
            echo '# ...'
            echo ''
            echo '# ALIASES'
            echo ''
            echo '# ...'
            echo ''
            echo '# GREET'
            echo ''
            echo 'greet'
            echo ''
        } > "${dst}"
        # If a new file was created but 'no' has been selected in prompt,
        # the file is brand new, hence echo message for user.
        if ! yn_is_yes; then
            echo "INFO: Created ${dst}"
        fi
    fi
}

symlink_all() {
    symlink_shell
    echo ""
    symlink_ssh
    echo ""
    symlink_git
    echo ""
    symlink_vscode
    echo ""
    symlink_alacritty
    echo ""
    symlink_termux
    echo ""
    symlink_r
}

create_all() {
    create_shell
    echo ""
    create_ssh
    echo ""
    create_git
    echo ""
    create_vscode
    echo ""
    create_alacritty
    echo ""
    create_termux
    echo ""
    create_r
}

update_dotfiles_path() {
    DOTFILES="$(dirname "$(pwd)"/"${0}")"
    export DOTFILES
    echo "INFO: DOTFILES='${DOTFILES}'"
}

# CMDLINE-PARSER

print_help() {
    echo "USAGE"
    echo "    ${0} [help|-h|--help] [-f|--force] [FUNCTION PARAM]"
    echo ""
    echo "DESCRIPTION"
    echo "    Kyle helps you with the setup."
    echo ""
    echo "    help -h --help"
    echo "        Print this help message."
    echo ""
    echo "    -f --force"
    echo "        Does not ask before replacing existing files."
    echo ""
    echo "    FUNCTION"
    echo "        create"
    echo "            Exports \${DOTFILES}. Further, creates and symlinks everything."
    echo "        symlink"
    echo "            Creates just the symlinks without the need of touching"
    echo "            \${DOTFILES}/custom/ (most in \${HOME})"
    echo ""
    echo "    OPTION"
    echo "        all"
    echo "            Exports \${DOTFILES}, creates and symlinks everything."
    echo "        shell"
    echo "            Creates and symlinks your shellrc."
    echo "        git"
    echo "            Creates a global git-config and symlinks a user-specific git-config."
    echo "        vscode"
    echo "            Creates and symlinks your configs for VSCode."
    echo "        alacritty"
    echo "            Creates and symlinks your alacritty-configs."
    echo "        termux"
    echo "            Creates and symlinks your termux-configs."
    echo "        R"
    echo "            Creates and symlinks your R-configs."
}

parse_cmdline() {
    if [ "${#}" -eq 0 ]; then
        print_help
        exit 0
    fi
    while [ "${#}" -gt 0 ]; do
        case "${1}" in
        '-h'|'--help'|'help')
            ERRCODE=0
            ;;
        'create'|'symlink')
            if [ -z "${2}" ]; then
                echo "ERROR: Missing value for function ${1}"
                echo ""
                ERRCODE=1
            else
                ARG_F_P="F_${1}_P_${2}"
                shift
                shift
            fi
            ;;
        '-f'|'--force')
            ARG_IS_FORCED=true
            shift
            ;;
        '-'*)
            echo "ERROR: Unknown argument ${1}"
            echo ""
            ERRCODE=1
            ;;
        *)
            echo "ERROR: Unknown function/value ${1}"
            echo ""
            ERRCODE=1
            ;;
        esac

        if [ -n "${ERRCODE}" ]; then
            print_help
            exit ${ERRCODE}
        fi
    done
}

main() {
    parse_cmdline "${@}"
    is_error=false
    case "${ARG_F_P}" in
    "F_${F_CREATE}_P_"*)
        update_dotfiles_path
        case "${ARG_F_P}" in
        "F_${F_CREATE}_P_${F_P_ALL}")       create_all   ;;
        "F_${F_CREATE}_P_${F_P_SHELL}")     create_shell ;;
        "F_${F_CREATE}_P_${F_P_GIT}")       create_git   ;;
        "F_${F_CREATE}_P_${F_P_VSCODE}")    create_vscode ;;
        "F_${F_CREATE}_P_${F_P_ALACRITTY}") create_alacritty ;;
        "F_${F_CREATE}_P_${F_P_TERMUX}")    create_termux ;;
        "F_${F_CREATE}_P_${F_P_R}")         create_r ;;
        *) is_error=true ;;
        esac
        ;;
    "F_${F_SYMLINK}_P_${F_P_ALL}")       symlink_all   ;;
    "F_${F_SYMLINK}_P_${F_P_SHELL}")     symlink_shell ;;
    "F_${F_SYMLINK}_P_${F_P_GIT}")       symlink_git   ;;
    "F_${F_SYMLINK}_P_${F_P_VSCODE}")    symlink_vscode ;;
    "F_${F_SYMLINK}_P_${F_P_ALACRITTY}") symlink_alacritty ;;
    "F_${F_SYMLINK}_P_${F_P_TERMUX}")    symlink_termux ;;
    "F_${F_SYMLINK}_P_${F_P_R}")         symlink_r ;;
    *) is_error=true ;;
    esac

    if "${is_error}"; then
        echo "ERROR: Unknown combi ${ARG_F_P}"
        print_help
        false
    fi
}

main "${@}"
