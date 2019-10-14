#!/usr/bin/env bash

#------------------------------------------------------------------------------#
# description of cmdline-parser

usage="
USAGE
    ${0} [OPTION]...

DESCRIPTION
       Creates the custom folder and uses drafts for non-existing files.
       The whole script is interactive and no file will be removed without permission.

       -h --help
              Print this help message
"

#------------------------------------------------------------------------------#
# check prerequisites

# exit as soon as error occurs
set -e

if [[ -z "${DOTFILES}" ]] || [[ ! -d "${DOTFILES}" ]]; then
    echo -e "ERROR: \${DOTFILES} is set incorrectly: ${DOTFILES}" >&2
    echo
    echo "${usage}"
    exit 1
fi

source "${DOTFILES}/utils/formatting.sh"

#------------------------------------------------------------------------------#
# cmdline-parser

while [[ "${#}" -gt 0 ]]; do
    case "${1}" in
    -h|--help)
        errcode=0
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
mkdir -p -v "${custom_dir}/"
# custom/git
mkdir -p -v "${custom_dir}/git/"
# custom/shell
mkdir -p -v "${custom_dir}/shell/"
mkdir -p -v "${custom_dir}/shell/func/"
mkdir -p -v "${custom_dir}/shell/ssh/"
# custom/vscode
mkdir -p -v "${custom_dir}/vscode/"
echo -e "${color_success}SUCCESS: custom-folders created${color_reset}"

#------------------------------------------------------------------------------#
# setup custom/git

echo -e "${color_info}INFO: Copying and linking git-files..${color_reset}"
echo -e "${color_info}INFO: ${HOME}/.gitconfig@ -> ${DOTFILES}/custom/git/config == ${DOTFILES}/git/config${color_reset}"
# copy dotfiles/git/config into custom
cp -i -P "${DOTFILES}/git/config" "${custom_dir}/git/config"
ln -i -s "${custom_dir}/git/config" "${HOME}/.gitconfig"
echo -e "${color_info}INFO: ${HOME}/.gitconfig.general@ -> ${DOTFILES}/custom/git/config.general@ -> ${DOTFILES}/git/config.general${color_reset}"
# link to config.general
ln -i -s "${DOTFILES}/git/config.general" "${custom_dir}/git/config.general"
ln -i -s "${custom_dir}/git/config.general" "${HOME}/.gitconfig.general"
echo -e "${color_success}SUCCESS: git-files configured${color_reset}"

#------------------------------------------------------------------------------#
# setup custom/shell

echo -e "${color_info}INFO: Creating and linking custom shellrc..${color_reset}"
file="${custom_dir}/shell/shellrc.sh"
# if already existing, ask for removing it 
if [[ -e "${file}" ]]; then
    echo -e "${color_warn}WARN: custom shellrc found -> remove and recreate?${color_reset}"
    rm -i "${file}"
fi
# If file has been removed or not existed yet
# -> create and fill it
# Further, echo default-shellrc since it depends on dynamic value ${DOTFILES}
if [[ ! -e "${file}" ]]; then
    echo "#$(yes \- | head -n78 | tr -d '[:space:]')#"        >  "${file}"
    echo "# If not running interactively, don't do anything"  >> "${file}"
    echo ''                                                   >> "${file}"
    echo 'case $- in'                                         >> "${file}"
    echo '    *i*) ;;'                                        >> "${file}"
    echo '      *) return ;;'                                 >> "${file}"
    echo 'esac'                                               >> "${file}"
    echo ''                                                   >> "${file}"
    echo "#$(yes \- | head -n78 | tr -d '[:space:]')#"        >> "${file}"
    echo '# setup'                                            >> "${file}"
    echo ''                                                   >> "${file}"
    echo "export DOTFILES=\"${DOTFILES}\""                    >> "${file}"
    echo ''                                                   >> "${file}"
    echo 'source "${DOTFILES}/shell/shellrc.sh"'              >> "${file}"
    echo ''                                                   >> "${file}"
    echo 'greet'                                              >> "${file}"
fi
# create symlinks in $HOME
echo -e "${color_info}INFO: ${HOME}/.profile@ -> ${custom_dir}/shell/shellrc.sh${color_reset}"
ln -i -s "${custom_dir}/shell/shellrc.sh" "${HOME}/.profile"
echo -e "${color_info}INFO: ${HOME}/.zshrc@ -> ${HOME}/.profile@${color_reset}"
ln -i -s "${HOME}/.profile" "${HOME}/.zshrc"
echo -e "${color_info}INFO: ${HOME}/.bashrc@ -> ${HOME}/.profile@${color_reset}"
ln -i -s "${HOME}/.profile" "${HOME}/.bashrc"
echo -e "${color_success}SUCCESS: shellrc configured${color_reset}"

#------------------------------------------------------------------------------#
# setup custom/shell/ssh

# create empty ssh-config in custom and link to it
echo -e "${color_info}INFO: Creating and linking custom ssh..${color_reset}"
echo -e "${color_info}INFO: ${HOME}/.ssh/config@ -> ${DOTFILES}/custom/shell/ssh/config${color_reset}"
file="${custom_dir}/shell/ssh/config"
# if already existing, ask for removing it 
if [[ -e "${file}" ]]; then
    echo -e "${color_warn}WARN: custom ssh-config found -> remove and recreate?${color_reset}"
    rm -i "${file}"
fi
# If file has been removed or not existed yet
# -> create and fill it
if [[ ! -e "${file}" ]]; then
    echo '# Add your ssh-configs here' > "${file}"
fi
# link: dotfiles/custom/shell/ssh/config <- ${HOME}/.ssh/config
mkdir -p -v "${HOME}/.ssh/"
ln -i -s "${file}" "${HOME}/.ssh/config"
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

# set vscode-links
echo -e "${color_info}INFO: ${VSCODE_HOME}/settings.json@ -> ${DOTFILES}/custom/vscode/settings.json@ -> ${DOTFILES}/vscode/settings.json${color_reset}"
ln -i -s "${dotfiles_vscode_dir}/settings.json" "${dotfiles_custom_vscode_dir}/settings.json"
ln -i -s "${dotfiles_custom_vscode_dir}/settings.json" "${VSCODE_HOME}/settings.json"
echo -e "${color_info}INFO: ${VSCODE_HOME}/keybindings.json@ -> ${DOTFILES}/custom/vscode/keybindings.json@ -> ${DOTFILES}/vscode/keybindings.json${color_reset}"
ln -i -s "${dotfiles_vscode_dir}/keybindings.json" "${dotfiles_custom_vscode_dir}/keybindings.json"
ln -i -s "${dotfiles_custom_vscode_dir}/keybindings.json" "${VSCODE_HOME}/keybindings.json"
echo -e "${color_success}SUCCESS: vscode-setup configured${color_reset}"
