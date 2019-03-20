################################################################################
# description of cmdline parser

_usage="
USAGE
    ${0} [OPTION]...

DESCRIPTION
       Creates the custom folder and uses drafts for non-existing files.

       -h --help
              Print this help message
"

################################################################################
# cmdline parser
# contains option parsing and help-message

while [[ "${#}" -gt 0 ]]; do
    case "${1}" in
    -h|--help)
        _errcode=0
        ;;
    esac

    if [[ ! -z "${_errcode}" ]]; then
        echo "${_usage}"
        exit ${_errcode}
    fi
done

################################################################################
# store symlink targets (folders and files)

# dirs in /custom/
_dirs=(
    "custom"
    "git"
    "shell"
    "shell/ssh"
    "vscode"
)

# files in /custom/
_files=(
    "create_gitignore.sh"
    "git/config"
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
