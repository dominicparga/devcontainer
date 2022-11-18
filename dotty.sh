#!/usr/bin/env sh

# DEFAULTS

f_create='create'
f_symlink='symlink'
f_help='help'
f_p_all='all'
f_p_shell='shell'
f_p_git='git'
f_p_vscodium='vscodium'
f_p_vscode='vscode'
f_p_termux='termux'
f_p_r='R'

arg_f_p="f_${f_create}_p_${f_p_all}"
arg_is_forced=false

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
    if "${arg_is_forced}"; then
        rm -rf "${new_file}"
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
            echo "INFO: Processed ${src}@"
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
            echo "INFO: Processed ${dst}"
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

set_vscodium_home() {
    dir="${HOME}/.config/VSCodium/User"
    unset vscodium_home
    if exists "${dir}"; then
        vscodium_home="${dir}"
    fi

    if [ -z "${vscodium_home}" ]; then
        echo "[ERROR] Could not find vscodium-folder"
        false
    else
        true
    fi
}

symlink_vscodium() {
    if set_vscodium_home; then
        safe_ln "${vscodium_home}/settings.json" "${DOTFILES}/custom/vscodium/settings.json"
        safe_ln "${vscodium_home}/keybindings.json" "${DOTFILES}/custom/vscodium/keybindings.json"
        safe_ln "${vscodium_home}/tasks.json" "${DOTFILES}/custom/vscodium/tasks.json"
        safe_ln "${vscodium_home}/snippets" "${DOTFILES}/custom/vscodium/snippets"
    fi
}

create_vscodium() {
    symlink_vscodium

    if set_vscodium_home; then
        safe_ln "${DOTFILES}/custom/vscodium/settings.json" "${DOTFILES}/src/vscodium/settings.json"
        safe_ln "${DOTFILES}/custom/vscodium/keybindings.json" "${DOTFILES}/src/vscodium/keybindings.json"
        safe_ln "${DOTFILES}/custom/vscodium/tasks.json" "${DOTFILES}/src/vscodium/tasks.json"
        safe_ln "${DOTFILES}/custom/vscodium/snippets" "${DOTFILES}/src/vscodium/snippets"
    fi
}

set_vscode_home() {
    # depends on OS, see
    # https://code.visualstudio.com/docs/getstarted/settings#_settings-file-locations
    dir="${HOME}/.config/Code/User"
    unset vscode_home
    if exists "${dir}"; then
        vscode_home="${dir}"
    fi

    if [ -z "${vscode_home}" ]; then
        echo "[ERROR] Could not find vscode-folder"
        false
    else
        true
    fi
}

symlink_vscode() {
    if set_vscode_home; then
        safe_ln "${vscode_home}/settings.json" "${DOTFILES}/custom/vscodium/settings.json"
        safe_ln "${vscode_home}/keybindings.json" "${DOTFILES}/custom/vscodium/keybindings.json"
        safe_ln "${vscode_home}/tasks.json" "${DOTFILES}/custom/vscodium/tasks.json"
        safe_ln "${vscode_home}/snippets" "${DOTFILES}/custom/vscodium/snippets"
    fi
}

create_vscode() {
    symlink_vscode

    if set_vscode_home; then
        safe_ln "${DOTFILES}/custom/vscodium/settings.json" "${DOTFILES}/src/vscodium/settings.json"
        safe_ln "${DOTFILES}/custom/vscodium/keybindings.json" "${DOTFILES}/src/vscodium/keybindings.json"
        safe_ln "${DOTFILES}/custom/vscodium/tasks.json" "${DOTFILES}/src/vscodium/tasks.json"
        safe_ln "${DOTFILES}/custom/vscodium/snippets" "${DOTFILES}/src/vscodium/snippets"
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
            echo "INFO: Processed ${dst}"
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
    symlink_vscodium
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
    create_vscodium
    echo ""
    create_termux
    echo ""
    create_r
}

update_dotfiles_path() {
    DOTFILES="$(dirname "$(realpath "${0}")")"
    export DOTFILES
    echo "[INFO] DOTFILES='${DOTFILES}'"
}

# CMDLINE-PARSER

print_help() {
    echo "USAGE"
    echo "    ${0} [${f_help}|-h|--${f_help}] [-f|--force] [FUNCTION PARAM]"
    echo ""
    update_dotfiles_path
    echo ""
    echo "DESCRIPTION"
    echo "    Dotty helps you with the setup."
    echo ""
    echo "    ${f_help} -h --${f_help}"
    echo "        Print this help message."
    echo ""
    echo "    -f --force"
    echo "        Does not ask before replacing existing files."
    echo ""
    echo "    FUNCTION"
    echo "        ${f_create}"
    echo "            Exports \${DOTFILES}. Further, creates and symlinks everything."
    echo "        ${f_symlink}"
    echo "            Creates just the symlinks without the need of touching"
    echo "            \${DOTFILES}/custom/ (most in \${HOME})"
    echo ""
    echo "    OPTION"
    echo "        ${f_p_all}"
    echo "            Exports \${DOTFILES}, creates and symlinks everything."
    echo "        ${f_p_shell}"
    echo "            Creates and symlinks your shellrc."
    echo "        ${f_p_git}"
    echo "            Creates a global git-config and symlinks a user-specific git-config."
    echo "        ${f_p_vscodium}"
    echo "            Creates and symlinks your configs for VSCodium."
    echo "        ${f_p_vscode}"
    echo "            Creates and symlinks your configs for VSCode."
    echo "        ${f_p_termux}"
    echo "            Creates and symlinks your termux-configs."
    echo "        ${f_p_r}"
    echo "            Creates and symlinks your R-configs."
}

parse_cmdline() {
    if [ "${#}" -eq 0 ]; then
        print_help
        exit 0
    fi
    while [ "${#}" -gt 0 ]; do
        case "${1}" in
        '-h'|"--${f_help}"|"${f_help}")
            errcode=0
            ;;
        'create'|'symlink')
            if [ -z "${2}" ]; then
                echo "[ERROR] Missing value for function ${1}"
                echo ""
                errcode=1
            else
                arg_f_p="f_${1}_p_${2}"
                shift
                shift
            fi
            ;;
        '-f'|'--force')
            arg_is_forced=true
            shift
            ;;
        '-'*)
            echo "[ERROR] Unknown argument ${1}"
            echo ""
            errcode=1
            ;;
        *)
            echo "[ERROR] Unknown function/value ${1}"
            echo ""
            errcode=1
            ;;
        esac

        if [ -n "${errcode}" ]; then
            print_help
            exit ${errcode}
        fi
    done
}

main() {
    parse_cmdline "${@}"
    is_error=false

    case "${arg_f_p}" in
    "f_${f_create}_p_"*)
        update_dotfiles_path
        case "${arg_f_p}" in
        "f_${f_create}_p_${f_p_all}")       create_all   ;;
        "f_${f_create}_p_${f_p_shell}")     create_shell ;;
        "f_${f_create}_p_${f_p_git}")       create_git   ;;
        "f_${f_create}_p_${f_p_vscodium}")  create_vscodium ;;
        "f_${f_create}_p_${f_p_vscode}")    create_vscode ;;
        "f_${f_create}_p_${f_p_termux}")    create_termux ;;
        "f_${f_create}_p_${f_p_r}")         create_r ;;
        *) is_error=true ;;
        esac
        ;;
    "f_${f_symlink}_p_${f_p_all}")       symlink_all   ;;
    "f_${f_symlink}_p_${f_p_shell}")     symlink_shell ;;
    "f_${f_symlink}_p_${f_p_git}")       symlink_git   ;;
    "f_${f_symlink}_p_${f_p_vscodium}")  symlink_vscodium ;;
    "f_${f_symlink}_p_${f_p_vscode}")    symlink_vscode ;;
    "f_${f_symlink}_p_${f_p_termux}")    symlink_termux ;;
    "f_${f_symlink}_p_${f_p_r}")         symlink_r ;;
    *) is_error=true ;;
    esac

    if "${is_error}"; then
        echo "[ERROR] Unknown combi ${arg_f_p}"
        print_help
        false
    fi
}

main "${@}"
