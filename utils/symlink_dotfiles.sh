#!/usr/bin/env bash

################################################################################
# check prerequisites

if [[ -z "${DOTFILES}" ]] || [[ ! -d "${DOTFILES}" ]]; then
    echo "Error: \$DOTFILES is set incorrectly." >&2
    echo
    echo "${_usage}"
    exit 1
fi

################################################################################
# masking
# Please edit cmdline parser below as well!

                #       added: cmdline parser  help msg
_mask_shell=1   # 001 - added:        y            y
_mask_git=2     # 010 - added:        y            y
_mask_vscode=4  # 100 - added:        y            y

_mask_all=7 # 2**n - 1
_mask_enable=${_mask_all}

################################################################################
# description of cmdline parser

_usage="
USAGE
    ${0} [OPTION]...

DESCRIPTION
       Setup the system by creating symlinks to these dotfiles and store them in
       your HOME.

       Each OPTION has a positive and a negative flag, which respectively mean
       'do install' or 'do not install'. All flags are parsed as they are given,
       so repeating a (no-)flag overwrites previous (no-)flags.
       Examples can be found below.

       h help
              Print this help message

       -f
              If the LINK_NAME does already exist, the existing file gets
              replaced silently.


       all (default)
              Sets all flags.

       no nothing
              Sets all no-flags.


       shell (default), no-shell
              Sets the links for .bashrc and .zshrc

       git (default), no-git
              Sets the links to the respective git configs.

       vscode (default), no-vscode
              Sets the links to the respective vscode settings.

EXAMPLES for flags
       shell
       no-shell shell
       shell no-shell shell
       will symlink shell files.

       no-shell
       shell shell no-shell
       will not symlink shell files.

       no shell
       will symlink only shell files.
"

################################################################################
# cmdline parser
# contains option parsing and help-message

while [[ "${#}" -gt 0 ]]; do
    case "${1}" in
# general
    h|help)
        _errcode=0
        ;;
    -f)
        _force="-f"
        shift
        ;;
# installs
    all)
        _mask_enable=${_mask_all}
        shift
        ;;
    no|nothing)
        _mask_enable=0
        shift
        ;;
    shell)
        _mask_enable=$((${_mask_enable} | ${_mask_shell}))
        shift
        ;;
    no-shell)
        _mask_enable=$((${_mask_enable} & (${_mask_all} - ${_mask_shell})))
        shift
        ;;
    git)
        _mask_enable=$((${_mask_enable} | ${_mask_git}))
        shift
        ;;
    no-git)
        _mask_enable=$((${_mask_enable} & (${_mask_all} - ${_mask_git})))
        shift
        ;;
    vscode)
        _mask_enable=$((${_mask_enable} | ${_mask_vscode}))
        shift
        ;;
    no-vscode)
        _mask_enable=$((${_mask_enable} & (${_mask_all} - ${_mask_vscode})))
        shift
        ;;
    -*)
        echo "Error: unknown option ${1}" >&2
        echo
        _errcode=1
        ;;
    *)
        echo "Error: unknown argument ${1}" >&2
        echo
        _errcode=1
        ;;
    esac

    if [[ ! -z "${_errcode}" ]]; then
        echo "${_usage}"
        exit ${_errcode}
    fi
done

################################################################################
# create custom folder

bash "${DOTFILES}/utils/create_custom.sh"

################################################################################
# all links will be processed in one loop

_targets=()
_links=()

################################################################################
# add shell references

if [[ ${_mask_shell} -eq $((${_mask_shell} & ${_mask_enable})) ]]; then
    _targets+=(
        "${DOTFILES}/custom/shell/shellrc.sh"
        "${HOME}/.profile"
        "${HOME}/.profile"
    )
    _links+=(
        "${HOME}/.profile"
        "${HOME}/.bashrc"
        "${HOME}/.zshrc"
    )

    # ssh
    if [[ ! -d "${HOME}/.ssh" ]]; then
        mkdir "${HOME}/.ssh"
    fi
    _targets+=("${DOTFILES}/custom/shell/ssh/config")
    _links+=("${HOME}/.ssh/config")
fi

################################################################################
# set git config links

if [[ ${_mask_git} -eq $((${_mask_git} & ${_mask_enable})) ]]; then
    _targets+=(
        "${DOTFILES}/custom/git/config"
        "${DOTFILES}/git/config"
    )
    _links+=(
        "${HOME}/.gitconfig"
        "${HOME}/.gitconfig.general"
    )
fi

################################################################################
# set links for vscode settings

if [[ ${_mask_vscode} -eq $((${_mask_vscode} & ${_mask_enable})) ]]; then
    # load function to determine machine later
    if [[ -f "${DOTFILES}/custom/shell/func/is_machine" ]]; then
        . "${DOTFILES}/custom/shell/func/is_machine"
    else
        . "${DOTFILES}/shell/func/is_machine"
    fi


    # set settings.json as target
    _targets+=(
        "${DOTFILES}/custom/vscode/settings.json"
        "${DOTFILES}/custom/vscode/keybindings.json"
    )


    # set links dependent of OS
    if ( is_machine "macOS" ); then
        _links+=(
            "${HOME}/Library/Application Support/Code/User/settings.json"
            "${HOME}/Library/Application Support/Code/User/keybindings.json"
        )
    elif ( is_machine "linux" ); then
        _links+=(
            "${HOME}/.config/Code/User/settings.json"
            "${HOME}/.config/Code/User/keybindings.json"
        )
    fi
fi

################################################################################
# create links

if [[ -z "${_force}" ]]; then
    # ask for permission

    for _link in "${_links[@]}"; do
        if [[ -f "${_link}" ]]; then
            REPLY="n"
            break
        fi
    done

    if [[ "${REPLY}" = "n" ]]; then
        echo "Some following files do already exist."
        for _link in "${_links[@]}"; do
            if [[ -f "${_link}" ]]; then
                echo "    ${_link}"
            fi
        done
        read -n 1 -p "Do you want to replace them (y/n)? "
        echo
    fi
fi

if [[ "${REPLY:-y}" =~ ^[yY]$ ]]; then
    for _i in "${!_links[@]}"; do
        ln -sf ${_force} "${_targets[${_i}]}" "${_links[${_i}]}"
    done
fi
