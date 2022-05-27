# dotfiles

[![Last commit](https://img.shields.io/github/last-commit/dominicparga/dotfiles?style=for-the-badge)](https://github.com/dominicparga/dotfiles/commits) [![License](https://img.shields.io/github/license/dominicparga/dotfiles?style=for-the-badge)](https://github.com/dominicparga/dotfiles/blob/nightly/LICENSE)


## Table of Contents

<!-- @import "[TOC]" {cmd="toc" depthFrom=1 depthTo=6 orderedList=false} -->

<!-- code_chunk_output -->

- [dotfiles](#dotfiles)
  - [Table of Contents](#table-of-contents)
  - [Dry and short](#dry-and-short)
  - [Usage](#usage)
    - [Configuration](#configuration)
    - [Change default location after configuration](#change-default-location-after-configuration)
  - [Contributing](#contributing)
  - [FAQ / Troubleshooting](#faq-troubleshooting)
    - [Syntax error](#syntax-error)
    - [Insecure files or directories](#insecure-files-or-directories)
    - [vscode-extensions doesn't work](#vscode-extensions-doesnt-work)
    - [vscode doesn't take my environment-variable](#vscode-doesnt-take-my-environment-variable)

<!-- /code_chunk_output -->


## Dry and short

1. Replace settings in `${HOME}` (`.gitconfig`, `.bashrc`, ...) by symlinks.
1. These symlinks in `${HOME}` point to symlinks in `dotfiles/custom`.
1. These symlinks in `dotfiles/custom` point to files in `dotfiles/src`.
1. Since `dotfiles/custom` is gitignored, you might replace these symlinks here.

`Visual Studio code` is also configured.
Following paths are linux-related (see [vscode-website](https://code.visualstudio.com/docs/getstarted/settings#_settings-file-locations)).

```shell
DOTFILES="${HOME}/dotfiles" # can be changed

# path here is linux-related
${HOME}/.config/Code/User/
├── settings.json@ -> ${DOTFILES}/custom/vscode/settings.json
└── keybindings.json@ -> ${DOTFILES}/custom/vscode/keybindings.json
```

These dotfiles are actively used with `archlinux`, `ubuntu`, `git-bash` (windows).


## Usage

```shell
cd ~
git clone https://github.com/dominicparga/dotfiles.git
./dotfiles/kyle.sh
```


### Configuration

You can setup your dotfiles using the provided `configure`-file (`macOS`, `linux`).
The `configure` initially sets `${DOTFILES}` dependent on its own location and calls `utils/configure.sh` creating the `custom`-folder and symlinks given the variable `${DOTFILES}` (set in `configure`).

Executing the following will create a folder `custom/` in the dotfiles folder and create all needed symlinks in there, but also in `${HOME}` and in `vscode`'s home (where `settings.json` lays, which is system-dependent).

```shell
# location of the dotfiles, probably in ${HOME}
cd ~

# if repo-name 'dotfiles' is okay
git clone https://github.com/dominicparga/dotfiles.git
cd dotfiles
# if another name should be used, e.g. dotties
git clone https://github.com/dominicparga/dotfiles.git dotties
cd dotties

./configure
```

Opening a new window will use the new setup.

> __Note:__ `custom/` contains some generic info that should probably be updated by hand (e.g. gitconfig's `user.name`).


### Change default location after configuration

You can change the default-location of your dotfiles easily by moving the dotfiles-folder around and executing `configure` again.

```shell
cd ~
mv dotfiles dotties
cd dotties
./configure
# Change hardcoded DOTFILES-path in `custom/shell/shellrc.sh`
```

Consider that `custom/shell/shellrc.sh` contains the location hardcoded after calling `configure`.
This is necessary to be fully custom here (e.g. moving `shellrc.sh` around).
If you keep your existing `custom/shell/shellrc.sh`, you have to change the hardcoded path manually.


## Contributing

👍🎉 First off, thanks for your interest in contributing to this project! 🎉👍

Please feel free writing issues or sending email to me.


## FAQ / Troubleshooting

Weird experiences from friends and others, which are using this repo, are mentioned below.


### Syntax error

These dotfiles are used with `bash` and `zsh`.
Check how `sh` is symlinked, e.g. via

```shell
ls -1GF --color=auto -lh -a $(which sh)
```

For instance, `Debian` uses `dash` instead of `bash` per default.
In this case, you have to change the system-shell with `chsh` or change the used shell in your terminal.


### Insecure files or directories

Your file permissions for your `dotfiles` are probably too loose.
Execute the following to set the permissions to `drwxr-xr-x`.

```shell
chmod -R 755 "${DOTFILES}"
```


### vscode-extensions doesn't work

You can downgrade installed extensions manually in vscode under `Extensions`.
Here, you click on the gear next to the installed and affected extension and select `Install Another Version...`.


### vscode doesn't take my environment-variable

`vscode`-settings can contain environment-dependent variables (e.g. `"key": "${env:VAR_NAME}"`).
Inside of `vscode`, a new terminal does source your `.profile`-file, but the `vscode`-window itself bases the shell-environment from its callee.
To use env-variables in `vscode`-settings, you set this variable in your shell before opening `vscode` with `code` (e.g. opening current folder with `code .`).
