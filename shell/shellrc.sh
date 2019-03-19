# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return ;;
esac

################################################################################
# initialization

if [[ -z "${DOTFILES}" ]]; then
    export DOTFILES="${HOME}/dotfiles"
fi
_shell_lib="${DOTFILES}/shell"
_custom_shell_lib="${DOTFILES}/custom/shell"

################################################################################
# sourcing

. "${_shell_lib}/autoloading.sh"
. "${_shell_lib}/history.sh"
. "${_shell_lib}/exports.sh"
. "${_shell_lib}/aliases.sh"
. "${_shell_lib}/prompts/left/short_path.sh"
. "${_shell_lib}/prompts/right/git_info.sh"

################################################################################
# customize shell

if [[ -f "${_custom_shell_lib}/extend_shellrc.sh" ]]; then
    . "${_custom_shell_lib}/extend_shellrc.sh"
fi

################################################################################
# cleanup

#unset DOTFILES
unset _shell_lib
unset _custom_shell_lib
