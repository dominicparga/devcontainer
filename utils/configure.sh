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

if [ -z "${DOTFILES}" ] || [ ! -d "${DOTFILES}" ]; then
    echo "ERROR: \${DOTFILES} is set incorrectly: ${DOTFILES}" >&2
    echo
    echo "${usage}"
    exit 1
fi

. "${DOTFILES}/utils/formatting.sh"

echo -e "${__COLOR_INFO}INFO: Setting \${DOTFILES} to '${DOTFILES}'${__COLOR_RESET}"

#------------------------------------------------------------------------------#
# cmdline-parser

while [ "${#}" -gt 0 ]; do
    case "${1}" in
    '-h'|'--help')
        __ERRCODE=0
        ;;
    '-'*)
        echo -e "${__COLOR_ERR}Unknown argument ${1}${__COLOR_RESET}"
        __ERRCODE=1
        ;;
    *)
        echo -e "${__COLOR_ERR}Unknown value ${1}${__COLOR_RESET}"
        __ERRCODE=1
        ;;
    esac

    if [ -n "${__ERRCODE}" ]; then
        echo -e "${__COLOR_INFO}${usage}${__COLOR_RESET}"
        exit ${__ERRCODE}
    fi
done

#------------------------------------------------------------------------------#
# create folders

echo -e "${__COLOR_INFO}INFO: Creating custom-folders..${__COLOR_RESET}"
__CUSTOM_DIR="${DOTFILES}/custom"

# custom
mkdir -p -v "${__CUSTOM_DIR}/"
# custom/alacritty
mkdir -p -v "${__CUSTOM_DIR}/alacritty/"
# custom/git
mkdir -p -v "${__CUSTOM_DIR}/git/"
# custom/shell
mkdir -p -v "${__CUSTOM_DIR}/shell/"
mkdir -p -v "${__CUSTOM_DIR}/shell/func/"
mkdir -p -v "${__CUSTOM_DIR}/shell/ssh/"
# custom/vscode
mkdir -p -v "${__CUSTOM_DIR}/vscode/"
echo -e "${__COLOR_SUCC}SUCCESS: custom-folders created${__COLOR_RESET}"
# custom/termux
mkdir -p -v "${__CUSTOM_DIR}/termux/"
# custom/R
mkdir -p -v "${__CUSTOM_DIR}/R/"

#------------------------------------------------------------------------------#
# setup custom/git

echo -e "${__COLOR_INFO}INFO: Copying and linking git-files..${__COLOR_RESET}"
# copy dotfiles/git/config into custom
echo -e "${__COLOR_INFO}INFO: ${HOME}/.gitconfig@ -> ${DOTFILES}/custom/git/config == ${DOTFILES}/git/config${__COLOR_RESET}"
cp -i -P "${DOTFILES}/git/config" "${__CUSTOM_DIR}/git/config"
ln -i -v -s "${__CUSTOM_DIR}/git/config" "${HOME}/.gitconfig"
# link to config.general
echo -e "${__COLOR_INFO}INFO: ${HOME}/.gitconfig.general@ -> ${DOTFILES}/custom/git/config.general@ -> ${DOTFILES}/git/config.general${__COLOR_RESET}"
ln -i -v -s "${DOTFILES}/git/config.general" "${__CUSTOM_DIR}/git/config.general"
ln -i -v -s "${__CUSTOM_DIR}/git/config.general" "${HOME}/.gitconfig.general"
echo -e "${__COLOR_SUCC}SUCCESS: git-files configured${__COLOR_RESET}"

#------------------------------------------------------------------------------#
# setup custom/shell

echo -e "${__COLOR_INFO}INFO: Creating and linking custom shellrc..${__COLOR_RESET}"
__FILE="${__CUSTOM_DIR}/shell/shellrc.sh"
# if already existing, ask for removing it
if [ -e "${__FILE}" ]; then
    echo -e "${__COLOR_WARN}WARN: custom shellrc found -> remove and recreate?${__COLOR_RESET}"
    rm -i -v "${__FILE}"
fi
# If file has been removed or not existed yet
# -> create and fill it
# Further, echo default-shellrc since it depends on dynamic value ${DOTFILES}
if [ ! -e "${__FILE}" ]; then
    touch "${__FILE}"
    {
        echo '#!/usr/bin/env sh';
        echo '';
        echo "#$(yes - | head -n78 | tr -d '[:space:]')#";
        echo "# If not running interactively, don't do anything";
        echo '';
        echo 'case $- in';
        echo '    *i*) ;;';
        echo '      *) return ;;';
        echo 'esac';
        echo '';
        echo "#$(yes - | head -n78 | tr -d '[:space:]')#";
        echo '# setup';
        echo '';
        echo "export DOTFILES=\"${DOTFILES}\"";
        echo '';
        echo '. "${DOTFILES}/shell/shellrc.sh"';
        echo '';
        echo 'greet';
        echo '';
    } > "${__FILE}"

    echo "created '${__FILE}'"
fi

# create symlinks in $HOME
echo -e "${__COLOR_INFO}INFO: ${HOME}/.profile@ -> ${__CUSTOM_DIR}/shell/shellrc.sh${__COLOR_RESET}"
ln -i -v -s "${__CUSTOM_DIR}/shell/shellrc.sh" "${HOME}/.profile"
echo -e "${__COLOR_INFO}INFO: ${HOME}/.zshrc@ -> ${HOME}/.profile@${__COLOR_RESET}"
ln -i -v -s "${HOME}/.profile" "${HOME}/.zshrc"
echo -e "${__COLOR_INFO}INFO: ${HOME}/.bashrc@ -> ${HOME}/.profile@${__COLOR_RESET}"
ln -i -v -s "${HOME}/.profile" "${HOME}/.bashrc"
echo -e "${__COLOR_SUCC}SUCCESS: shellrc configured${__COLOR_RESET}"

#------------------------------------------------------------------------------#
# setup custom/shell/ssh

# create empty ssh-config in custom and link to it
echo -e "${__COLOR_INFO}INFO: Creating and linking custom ssh..${__COLOR_RESET}"
echo -e "${__COLOR_INFO}INFO: ${HOME}/.ssh/config@ -> ${DOTFILES}/custom/shell/ssh/config${__COLOR_RESET}"
__FILE="${__CUSTOM_DIR}/shell/ssh/config"
# if already existing, ask for removing it
if [ -e "${__FILE}" ]; then
    echo -e "${__COLOR_WARN}WARN: custom ssh-config found -> remove and recreate?${__COLOR_RESET}"
    rm -i -v "${__FILE}"
fi
# If file has been removed or not existed yet
# -> create and fill it
if [ ! -e "${__FILE}" ]; then
    touch "${__FILE}"
    echo '# Add your ssh-configs here' > "${__FILE}"
    echo "created '${__FILE}'"
fi
# link: dotfiles/custom/shell/ssh/config <- ${HOME}/.ssh/config
mkdir -p -v "${HOME}/.ssh/socket"
ln -i -v -s "${__FILE}" "${HOME}/.ssh/config"
chmod 600 "${HOME}/.ssh/config"

# link: dotfiles/custom/shell/ssh/config <- ${HOME}/.ssh/config
__FILE="${DOTFILES}/shell/ssh-find-agent.sh"
ln -i -v -s "${__FILE}" "${HOME}/.ssh-find-agent"

echo -e "${__COLOR_SUCC}SUCCESS: ssh configured${__COLOR_RESET}"

#------------------------------------------------------------------------------#
# setup custom/alacritty

echo -e "${__COLOR_INFO}INFO: Copying and linking alacritty-files..${__COLOR_RESET}"
__FILE="${HOME}/.config/alacritty/alacritty.yml"
if [ -e "${__FILE}" ]; then
    echo -e "${__COLOR_WARN}WARN: ${__FILE} found -> remove?${__COLOR_RESET}"
    rm -i -v "${__FILE}"
fi
echo -e "${__COLOR_INFO}INFO: ${HOME}/.alacritty.yml@ -> ${__CUSTOM_DIR}/alacritty/alacritty.yml -> ${DOTFILES}/alacritty/alacritty.yml${__COLOR_RESET}"
ln -i -v -s "${DOTFILES}/alacritty/alacritty.yml" "${__CUSTOM_DIR}/alacritty/alacritty.yml"
ln -i -v -s "${__CUSTOM_DIR}/alacritty/alacritty.yml" "${HOME}/.alacritty.yml"
echo -e "${__COLOR_SUCC}SUCCESS: alacritty-files configured${__COLOR_RESET}"

#------------------------------------------------------------------------------#
# setup custom/vscode

echo -e "${__COLOR_INFO}INFO: Creating and linking vscode-setup..${__COLOR_RESET}"

# Set __VSCODE_HOME dependent of system.
# See https://code.visualstudio.com/docs/getstarted/settings#_settings-file-locations

# macOS
# linux - microsoft-release
# linux - open-source-release
for __DIR in \
    "${HOME}/Library/Application Support/Code/User" \
    "${HOME}/.config/Code/User" \
    "${HOME}/.config/Code - OSS/User"
do
    if [ -e "${__DIR}" ]; then
        __VSCODE_HOME="${__DIR}"
        break
    fi
done
# check if var is set, which means the directory exists
if [ -n "${__VSCODE_HOME}" ]; then
    __DOTFILES_VSCODE_DIR="${DOTFILES}/vscode"
    __DOTFILES_CUSTOM_VSCODE_DIR="${__CUSTOM_DIR}/vscode"

    # set vscode-links
    echo -e "${__COLOR_INFO}INFO: ${__VSCODE_HOME}/settings.json@ -> ${DOTFILES}/custom/vscode/settings.json@ -> ${DOTFILES}/vscode/settings.json${__COLOR_RESET}"
    ln -i -v -s "${__DOTFILES_VSCODE_DIR}/settings.json" "${__DOTFILES_CUSTOM_VSCODE_DIR}/settings.json"
    ln -i -v -s "${__DOTFILES_CUSTOM_VSCODE_DIR}/settings.json" "${__VSCODE_HOME}/settings.json"
    echo -e "${__COLOR_INFO}INFO: ${__VSCODE_HOME}/keybindings.json@ -> ${DOTFILES}/custom/vscode/keybindings.json@ -> ${DOTFILES}/vscode/keybindings.json${__COLOR_RESET}"
    ln -i -v -s "${__DOTFILES_VSCODE_DIR}/keybindings.json" "${__DOTFILES_CUSTOM_VSCODE_DIR}/keybindings.json"
    ln -i -v -s "${__DOTFILES_CUSTOM_VSCODE_DIR}/keybindings.json" "${__VSCODE_HOME}/keybindings.json"
    echo -e "${__COLOR_SUCC}SUCCESS: vscode-setup configured${__COLOR_RESET}"
else
    echo -e "${__COLOR_WARN}WARN: Could not find vscode-folder${__COLOR_RESET}"
fi


#------------------------------------------------------------------------------#
# setup custom/termux

if [ -n "${HOME}/.termux" ]; then
    echo -e "${__COLOR_INFO}INFO: Copying and linking termux-files..${__COLOR_RESET}"
    # copy dotfiles/termux.properties into custom
    echo -e "${__COLOR_INFO}INFO: ${HOME}/.termux/termux.properties -> ${DOTFILES}/custom/termux/properties -> ${DOTFILES}/termux/properties${__COLOR_RESET}"
    cp -i -P "${DOTFILES}/termux/properties" "${__CUSTOM_DIR}/termux/properties"
    ln -i -v -s "${DOTFILES}/termux/properties" "${__CUSTOM_DIR}/termux/properties"
    ln -i -v -s "${__CUSTOM_DIR}/termux/properties" "${HOME}/.termux/termux.properties"
    echo -e "${__COLOR_SUCC}SUCCESS: termux-files configured${__COLOR_RESET}"
else
    echo -e "${__COLOR_WARN}WARN: Could not find .termux in home-directory${__COLOR_RESET}"
fi


#------------------------------------------------------------------------------#
# setup custom/R

echo -e "${__COLOR_INFO}INFO: Copying and linking R-files..${__COLOR_RESET}"
# copy dotfiles/R/profile.R into custom
echo -e "${__COLOR_INFO}INFO: ${HOME}/.Rprofile@ -> ${DOTFILES}/custom/R/profile.R -> ${DOTFILES}/R/profile.R${__COLOR_RESET}"
cp -i -P "${DOTFILES}/R/profile.R" "${__CUSTOM_DIR}/R/profile.R"
ln -i -v -s "${DOTFILES}/R/profile.R" "${__CUSTOM_DIR}/R/profile.R"
ln -i -v -s "${__CUSTOM_DIR}/R/profile.R" "${HOME}/.Rprofile"
# link to environ.sh
echo -e "${__COLOR_INFO}INFO: ${HOME}/.Renviron@ -> ${DOTFILES}/custom/R/environ.sh@ -> ${DOTFILES}/R/environ.sh${__COLOR_RESET}"
ln -i -v -s "${DOTFILES}/R/environ.sh" "${__CUSTOM_DIR}/R/environ.sh"
ln -i -v -s "${__CUSTOM_DIR}/R/environ.sh" "${HOME}/.Renviron"
echo -e "${__COLOR_SUCC}SUCCESS: R-files configured${__COLOR_RESET}"

#------------------------------------------------------------------------------#
# setup custom/emacs

echo -e "${__COLOR_INFO}INFO: Creating and linking emacs-setup..${__COLOR_RESET}"

# Set __VSCODE_HOME dependent of system.
# See https://code.visualstudio.com/docs/getstarted/settings#_settings-file-locations

__EMACS_HOME="${HOME}"
__DOT_EMACS="${HOME}/.emacs"
__DOT_EMACS_D="${HOME}/.emacs.d"

__DOTFILES_EMACS_DIR="${DOTFILES}/emacs"

# set emacs links
echo -e "${__COLOR_INFO}INFO: ${__EMACS_HOME}/.emacs@ -> ${__DOTFILES_EMACS_DIR}/emacs${__COLOR_RESET}"
ln -i -v -s "${__DOTFILES_EMACS_DIR}/init.el" "${__DOT_EMACS}"
echo -e "${__COLOR_INFO}INFO: ${__EMACS_HOME}/.emacs.d@ -> ${__DOTFILES_EMACS_DIR}${__COLOR_RESET}"
mkdir -p "${__DOT_EMACS_D}"
ln -i -v -s "${__DOTFILES_EMACS_DIR}/elisp-local" "${__DOT_EMACS_D}"
echo -e "${__COLOR_SUCC}SUCCESS: emacs-setup configured${__COLOR_RESET}"
