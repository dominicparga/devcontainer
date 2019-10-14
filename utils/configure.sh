#!/usr/bin/env bash

#------------------------------------------------------------------------------#
# description of cmdline-parser

usage="
USAGE
    ${0} [OPTION]...

DESCRIPTION
       Creates the custom folder and uses drafts for non-existing files.
       Default flags: -i

       -i --interactive
              prompt whether to remove or replace existing files.
              Overrides a previous -f option.

       -f --force
              do overwrite an existing file silently!
              Overrides a previous -i option.

       -h --help
              Print this help message
"

#------------------------------------------------------------------------------#
# check prerequisites

if [[ -z "${DOTFILES}" ]] || [[ ! -d "${DOTFILES}" ]]; then
    echo -e "ERROR: \$DOTFILES is set incorrectly." >&2
    echo
    echo "${usage}"
    exit 1
fi

source "${DOTFILES}/utils/formatting.sh"

#------------------------------------------------------------------------------#
# cmdline-parser

cp_ln_args='-i'
while [[ "${#}" -gt 0 ]]; do
    case "${1}" in
    -h|--help)
        errcode=0
        ;;
    -i|--interactive)
        shift
        cp_ln_args='-i'
        ;;
    -f|--force)
        shift
        cp_ln_args='-f'
        ;;
    -*)
        echo -e "${color_error}Unknown argument ${1}${color_reset}"
        errcode=1
        ;;
    *)
        echo -e "${color_error}Unknown value ${1}${color_reset}"
        errcode=1
        ;;
    esac

    if [[ -n "${errcode}" ]]; then
        echo -e "${color_info}${usage}${color_reset}"
        exit ${errcode}
    fi
done

#------------------------------------------------------------------------------#
# create folders

echo -e "${color_info}INFO: Creating custom-folders..${color_reset}"
custom_dir="${DOTFILES}/custom"

# custom
mkdir "${custom_dir}/" 2> /dev/null
# custom/git
mkdir "${custom_dir}/git/" 2> /dev/null
# custom/shell
mkdir "${custom_dir}/shell/" 2> /dev/null
mkdir "${custom_dir}/shell/func/" 2> /dev/null
mkdir "${custom_dir}/shell/ssh/" 2> /dev/null
# custom/vscode
mkdir "${custom_dir}/vscode/" 2> /dev/null
echo -e "${color_success}SUCCESS: custom-folders created${color_reset}"

#------------------------------------------------------------------------------#
# setup custom/git

echo -e "${color_info}INFO: Copying and linking git-files..${color_reset}"
echo -e "${color_info}INFO: ~/.gitconfig@ -> \${DOTFILES}/custom/git/config == \${DOTFILES}/git/config${color_reset}"
# copy dotfiles/git/config into custom
cp "${cp_ln_args}" -P "${DOTFILES}/git/config" "${custom_dir}/git/config"
ln "${cp_ln_args}" -s "${custom_dir}/git/config" "${HOME}/.gitconfig"
echo -e "${color_info}INFO: ~/.gitconfig.general@ -> \${DOTFILES}/custom/git/config.general@ -> \${DOTFILES}/git/config.general${color_reset}"
# link to config.general
ln "${cp_ln_args}" -s "${DOTFILES}/git/config.general" "${custom_dir}/git/config.general"
ln "${cp_ln_args}" -s "${custom_dir}/git/config.general" "${HOME}/.gitconfig.general"
echo -e "${color_success}SUCCESS: git-files configured${color_reset}"

#------------------------------------------------------------------------------#
# setup custom/shell

echo -e "${color_info}INFO: Creating and linking custom shellrc..${color_reset}"
# echo default shellrc since it depends on dynamic value ${DOTFILES}
file="${custom_dir}/shell/shellrc.sh"
new_file="${custom_dir}/shell/new_shellrc.sh"
if [[ ! -e "${new_file}" ]]; then
    echo "#$(yes \- | head -n78 | tr -d '[:space:]')#"         >  "${new_file}"
    echo "# If not running interactively, don't do anything"  >> "${new_file}"
    echo ''                                                   >> "${new_file}"
    echo 'case $- in'                                         >> "${new_file}"
    echo '    *i*) ;;'                                        >> "${new_file}"
    echo '      *) return ;;'                                 >> "${new_file}"
    echo 'esac'                                               >> "${new_file}"
    echo ''                                                   >> "${new_file}"
    echo "#$(yes \- | head -n78 | tr -d '[:space:]')#"        >> "${new_file}"
    echo '# setup'                                            >> "${new_file}"
    echo ''                                                   >> "${new_file}"
    echo "export DOTFILES=\"${DOTFILES}\""                    >> "${new_file}"
    echo ''                                                   >> "${new_file}"
    echo 'source "${DOTFILES}/shell/shellrc.sh"'              >> "${new_file}"
    echo ''                                                   >> "${new_file}"
    echo 'greet'                                              >> "${new_file}"
fi
cp "${cp_ln_args}" -P "${new_file}" "${file}"
rm "${new_file}"
echo -e "${color_success}SUCCESS: shellrc configured${color_reset}"

echo -e "${color_info}INFO: Creating and linking custom ssh..${color_reset}"
echo -e "${color_info}INFO: ~/.ssh/config@ -> \${DOTFILES}/custom/shell/ssh/config${color_reset}"
# create empty ssh-config in custom and link to it
file="${custom_dir}/shell/ssh/config"
new_file="${custom_dir}/shell/ssh/new_config"
if [[ ! -e "${new_file}" ]]; then
    echo '# Add your ssh configs here' > "${new_file}"
fi
cp "${cp_ln_args}" -P "${new_file}" "${file}"
rm "${new_file}"
mkdir "${HOME}/.ssh/" 2> /dev/null
# dotfiles/custom/shell/ssh/config <- ~/.ssh/config
ln "${cp_ln_args}" -s "${file}" "${HOME}/.ssh/config"
echo -e "${color_success}SUCCESS: ssh configured${color_reset}"

#------------------------------------------------------------------------------#
# setup custom/vscode

echo -e "${color_info}INFO: Creating and linking vscode-setup..${color_reset}"
# Set VSCODE_HOME dependent of system.
# See https://code.visualstudio.com/docs/getstarted/settings#_settings-file-locations
source "${DOTFILES}/shell/func/is_machine"
if ( is_machine 'macOS' ); then
    VSCODE_HOME="${HOME}/Library/Application Support/Code/User"
elif ( is_machine 'linux' ); then
    VSCODE_HOME="${HOME}/.config/Code/User"
else
    echo -e "${color_error}ERROR: Could not find vscode-folder due to unknown system${color_reset}"
    exit 1
fi
dotfiles_vscode_dir="${DOTFILES}/vscode"
dotfiles_custom_vscode_dir="${custom_dir}/vscode"

echo -e "${color_info}INFO: \${VSCODE_HOME}/settings.json@ -> \${DOTFILES}/custom/vscode/settings.json@ -> \${DOTFILES}/vscode/settings.json${color_reset}"
ln "${cp_ln_args}" -s "${dotfiles_vscode_dir}/settings.json" "${dotfiles_custom_vscode_dir}/settings.json"
ln "${cp_ln_args}" -s "${dotfiles_custom_vscode_dir}/settings.json" "${VSCODE_HOME}/settings.json"
echo -e "${color_info}INFO: \${VSCODE_HOME}/keybindings.json@ -> \${DOTFILES}/custom/vscode/keybindings.json@ -> \${DOTFILES}/vscode/keybindings.json${color_reset}"
ln "${cp_ln_args}" -s "${dotfiles_vscode_dir}/keybindings.json" "${dotfiles_custom_vscode_dir}/keybindings.json"
ln "${cp_ln_args}" -s "${dotfiles_custom_vscode_dir}/keybindings.json" "${VSCODE_HOME}/keybindings.json"
echo -e "${color_success}SUCCESS: vscode-setup configured${color_reset}"
