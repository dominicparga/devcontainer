################################################################################
# description of cmdline parser

_usage="
USAGE
    ${0} [OPTION]...

DESCRIPTION
       Creates the custom folder and uses drafts for non-existing files.

       h help
              Print this help message
"

################################################################################
# check prerequisites

if [[ -z "${DOTFILES}" ]] || [[ ! -d "${DOTFILES}" ]]; then
    echo "Error: \$DOTFILES is set incorrectly." >&2
    echo
    echo "${_usage}"
    exit 1
fi

################################################################################
# cmdline parser
# contains option parsing and help-message

while [[ "${#}" -gt 0 ]]; do
    case "${1}" in
    h|help)
        _errcode=0
        ;;
    esac

    if [[ -n "${_errcode}" ]]; then
        echo "${_usage}"
        exit ${_errcode}
    fi
done

################################################################################
# store symlink targets (folders and files)

# dirs in /custom/
_dirs=(
    "custom"
    "install"
    "install/macOS"
    "install/python"
    "install/ubuntu"
    "install/vscode"
    "git"
    "shell"
    "shell/func"
    "shell/ssh"
    "vscode"
)

# files in /custom/
_files=(
    "create_gitignore.sh"
    "git/config"
    "install/macOS/brew_formulae.sh"
    "install/python/pkgs.sh"
    "install/ubuntu/apt_pkgs.sh"
    "install/ubuntu/nord_style.sh"
    "install/ubuntu/npm.sh"
    "install/ubuntu/vscode.sh"
    "install/vscode/extensions.sh"
    "shell/ssh/config"
    "vscode/settings.json"
    "vscode/keybindings.json"
)

_custom_dir="${DOTFILES}/custom"
_drafts_dir="${DOTFILES}/utils/drafts"

################################################################################
# create folders

_sth_printed=1

if [[ ! -d "${_custom_dir}" ]]; then
    mkdir -v "${_custom_dir}"
    _sth_printed=0
fi

for _dir in "${_dirs[@]}"; do
    if [[ ! -d "${_custom_dir}/${_dir}" ]]; then
        mkdir -v "${_custom_dir}/${_dir}"
        _sth_printed=0
    fi
done

if [ "${_sth_printed}" ]; then
    echo
fi

################################################################################
# create files

_sth_printed=1

for _file in "${_files[@]}"; do
    if [[ ! -f "${_custom_dir}/${_file}" ]]; then
        ########################################################################
        # file dependent content -> copy drafts

        if [[ -f "${_drafts_dir}/${_file}" ]]; then
            cp -v "${_drafts_dir}/${_file}" "${_custom_dir}/${_file}"
        else
            touch "${_custom_dir}/${_file}"
        fi
    fi
done

if [[ "${_sth_printed}" -eq 0 ]]; then
    echo
fi

################################################################################
# create custom shellrc

_file="${DOTFILES}/custom/shell/shellrc.sh"
if [[ ! -f "${_file}" ]]; then
    echo "$(yes \# | head -n80 | tr -d '[:space:]')"          >  "${_file}"
    echo "# If not running interactively, don't do anything"  >> "${_file}"
    echo ''                                                   >> "${_file}"
    echo 'case $- in'                                         >> "${_file}"
    echo '    *i*) ;;'                                        >> "${_file}"
    echo '      *) return ;;'                                 >> "${_file}"
    echo 'esac'                                               >> "${_file}"
    echo ''                                                   >> "${_file}"
    echo "$(yes \# | head -n80 | tr -d '[:space:]')"          >> "${_file}"
    echo '# setup'                                            >> "${_file}"
    echo ''                                                   >> "${_file}"
    echo "export DOTFILES=\"${DOTFILES}\""                    >> "${_file}"
    echo ''                                                   >> "${_file}"
    echo '. "${DOTFILES}/shell/shellrc.sh"'                   >> "${_file}"
    echo ''                                                   >> "${_file}"
    echo 'greet'                                              >> "${_file}"
fi
