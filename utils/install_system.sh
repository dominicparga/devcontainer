#!/usr/bin/env sh

################################################################################
# masking
# Please edit cmdline parser below as well!

                     #           added: cmdline parser  help msg
_mask_apt=1          # 0000001 - added:        y            y
_mask_brew=2         # 0000010 - added:        y            y
_mask_nord=4         # 0000100 - added:        y            y
_mask_npm=4          # 0001000 - added:        y            y
_mask_py_pkgs=8      # 0010000 - added:        y            y
_mask_vscode=16      # 0100000 - added:        y            y
_mask_vscode_ext=32  # 1000000 - added:        y            y


# load function to determine machine
if [[ -f "${DOTFILES}/custom/shell/func/is_machine" ]]; then
    . "${DOTFILES}/custom/shell/func/is_machine"
else
    . "${DOTFILES}/shell/func/is_machine"
fi



# set white mask dependent on machine
_mask_all=0  # full would be (2**n - 1)

if ( is_machine "macOS" ); then
    _mask_all=$((${_mask_all} | ${_mask_brew}))
    _mask_all=$((${_mask_all} | ${_mask_py_pkgs}))
    _mask_all=$((${_mask_all} | ${_mask_vscode_ext}))
elif ( is_machine "Linux" ); then
    _mask_all=$((${_mask_all} | ${_mask_apt}))
    _mask_all=$((${_mask_all} | ${_mask_npm}))
    _mask_all=$((${_mask_all} | ${_mask_vscode}))
    _mask_all=$((${_mask_all} | ${_mask_vscode_ext}))
fi

# init enabled mask
_mask_enable=$((${_mask_all}))

################################################################################
# description of cmdline parser

_usage="
USAGE
    ${0} [OPTION]...

DESCRIPTION
       Setup your system. This includes:
       - installing all packages (macOS: homebrew, ubuntu: apt)
       - installing python packages (pip3 and pip2)
       - installing vscode and extensions

       Each OPTION has a positive and a negative flag, which respectively mean
       'do install' or 'do not install'. All flags are parsed as they are given,
       so repeating a (no-)flag overwrites previous (no-)flags.
       Examples can be found below.

       h help
              Print this help message


       all (default)
              Sets all possible flags dependent of the system:

              macOS
              no brew py-pkgs vscode-ext

              ubuntu
              no apt vscode vscode-ext

       nothing
              Sets all no-flags.


       apt (default), no-apt
              Installs all needed apt packages, including python packages.

       brew (default), no-brew
              Installs all needed brew formulae.

       nord, no-nord (default)
              Installs the nord style for ubuntu's terminal.

       npm (default), no-nord
              Installs npm for ubuntu.

       py-pkgs (macOS: default), no-py-pkgs (linux: default)
              Installs all needed py packages.

       vscode (linux: default), no-vscode (macOS: default)
              Installs visual studio code

       vscode-ext (default), no-vscode-ext
              Installs specified vscode extensions.

EXAMPLES for flags
       brew
       no-brew brew
       brew no-brew brew
       will install brew packages.

       no-brew
       brew brew no-brew
       will not install brew packages.

       no brew
       will install only brew packages.

"

################################################################################
# cmdline parser
# contains option parsing and help-message

# options in ALPHABETICAL order, separated in general flags and others
while [[ "${#}" -gt 0 ]]; do
    case "${1}" in
    # general
    all)
        _mask_enable=${_mask_all}
        shift
        ;;
    no|nothing)
        _mask_enable=0
        shift
        ;;
    h|help)
        _errcode=0
        ;;
    # others
    apt)
        _mask_enable=$((${_mask_enable} | ${_mask_apt}))
        shift
        ;;
    no-apt)
        _mask_enable=$((${_mask_enable} & (${_mask_all} - ${_mask_apt})))
        shift
        ;;
    brew)
        _mask_enable=$((${_mask_enable} | ${_mask_brew}))
        shift
        ;;
    no-brew)
        _mask_enable=$((${_mask_enable} & (${_mask_all} - ${_mask_brew})))
        shift
        ;;
    nord)
        _mask_enable=$((${_mask_enable} | ${_mask_nord}))
        shift
        ;;
    no-nord)
        _mask_enable=$((${_mask_enable} & (${_mask_all} - ${_mask_nord})))
        shift
        ;;
    npm)
        _mask_enable=$((${_mask_enable} | ${_mask_npm}))
        shift
        ;;
    no-npm)
        _mask_enable=$((${_mask_enable} & (${_mask_all} - ${_mask_npm})))
        shift
        ;;
    py-pkgs)
        _mask_enable=$((${_mask_enable} | ${_mask_py_pkgs}))
        shift
        ;;
    no-py-pkgs)
        _mask_enable=$((${_mask_enable} & (${_mask_all} - ${_mask_py_pkgs})))
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
    vscode-ext)
        _mask_enable=$((${_mask_enable} | ${_mask_vscode_ext}))
        shift
        ;;
    no-vscode-ext)
        _mask_enable=$((${_mask_enable} & (${_mask_all} - ${_mask_vscode_ext})))
        shift
        ;;
    *)
        echo "hello_${1}_bye"
        echo "Error: unknown argument ${1}" >&2
        echo
        _errcode=1
        ;;
    esac

    # filter OS
    _mask_enable=$((${_mask_enable} & ${_mask_all}))

    # error?
    if [[ ! -z "${_errcode}" ]]; then
        echo "${_usage}"
        exit ${_errcode}
    fi
done

################################################################################
# install apt packages

if [[ ${_mask_apt} -eq $((${_mask_apt} & ${_mask_enable})) ]]; then
    sh "${DOTFILES}/custom/install/ubuntu/apt_pkgs.sh"
    echo
fi

################################################################################
# install brew formulae

if [[ ${_mask_brew} -eq $((${_mask_brew} & ${_mask_enable})) ]]; then
    sh "${DOTFILES}/custom/install/macOS/brew_formulae.sh"
    echo
fi

################################################################################
# install python-packages

if [[ ${_mask_py_pkgs} -eq $((${_mask_py_pkgs} & ${_mask_enable})) ]]; then
    sh "${DOTFILES}/custom/install/python/pkgs.sh"
    echo
fi

################################################################################
# install nord style

if [[ ${_mask_nord} -eq $((${_mask_nord} & ${_mask_enable})) ]]; then
    sh "${DOTFILES}/custom/install/ubuntu/nord_style.sh"
    echo
fi

################################################################################
# install npm

if [[ ${_mask_npm} -eq $((${_mask_npm} & ${_mask_enable})) ]]; then
    sh "${DOTFILES}/custom/install/ubuntu/npm.sh"
fi

################################################################################
# install visual-studio-code

if [[ ${_mask_vscode} -eq $((${_mask_vscode} & ${_mask_enable})) ]]; then
    sh "${DOTFILES}/custom/install/ubuntu/vscode.sh"
    echo
fi

################################################################################
# install visual-studio-code extensions

if [[ ${_mask_vscode_ext} -eq $((${_mask_vscode_ext} & ${_mask_enable})) ]]; then
    sh "${DOTFILES}/custom/install/vscode/extensions.sh"
    echo
fi
