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
  - [Features](#features)
    - [Shell environment](#shell-environment)
    - [Git aliases](#git-aliases)
  - [Structure](#structure)
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

These dotfiles are used and tested with `archlinux`.


## Usage

```shell
cd ~
git clone https://github.com/dominicparga/dotfiles.git
./dotfiles/configure.bash
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


## Features

Besides some handy functions and a clean way of structuring your dotfiles, useful aliases and functions are provided.
These dotfiles are used with `bash` and `zsh` on `macOS` and some `linux`-distributions.


### Shell environment

Beside autocompletion for several tools, aliases and exports can be found in [`shell/shellrc.sh`](https://github.com/dominicparga/dotfiles/blob/nightly/shell/shellrc.sh).
Some commands like `grep` or `tree` are just flagged to use colors.
Those are not mentioned here.

| new cmd | note |
|:---------:|------|
| SAFETY ALIASES |
| `cp` | is alias for `cp -i -P`, so asks before replacing existing files |
| `mv` | is alias for `mv -i`, so asks before replacing existing files |
| COMMAND ls |
| `l` | shows items as a colored list |
| `la` | shows hidden items as well |
| `ll` | shows items as a colored list and access rights |
| `lla` | shows hidden items as well |
| WORKING QUICKLY |
| `c` | is alias for `clear` |
| `g` | is alias for `git` |
| `..` | is alias for `cd ..` |
| `.2` or `...` | goes back 2 folders |
| `.3` or `....` | goes back 3 folders |
| ... | ... |
| `.6` or `.......` | goes back 6 folders |
| `mkd` | creates folder(s) and enters it |
| MORE HANDY FUNCTIONS |
| `alert` | can be called like `sleep 2; alert` |
| `gitignore` | uses the `gitignore.io` API to echo gitignore entries, which can be piped into a `.gitignore`-file |


### Git aliases

Have a look at the handy [git aliases](https://github.com/dominicparga/dotfiles/blob/nightly/git/config.general).
In addition, visual-studio-code is opening as diff-tool and for commit-messages.

`g` is alias for `git` (see above).

| alias | note |
|:-----:|------|
| GENERAL |
| `g h` | helps using `git help` more often ;) (`g h ALIAS` shows the replacement for the alias). |
| `g s` | is alias for `git status` and one of the most used aliases. |
| `g hash` | is alias for `git rev-parse --verify HEAD` and returns the commit-hash, where HEAD is pointing at. |
| `g unstage FILES` | removes all changes from the index with respect to the given FILES (but keeps the changes in workspace). Simply spoken, all green FILES in `git status` become red again. |
| `g discard FILES` | removes all changes from the workspace with respect to the given FILES. Simply spoken, all red FILES in `git status` disappear. (__ATTENTION!__ Obviously, those changes will be lost.) |
| `g undo` | removes the last commit from history, but keeps its changes in the index. __ATTENTION!__ This alias can be very handy but it is recommended using this only for local commits, since removing pushed commits messes up the history. |
| COMMITTING |
| `g a FILES` | adds the given files. |
| `g aa` | adds all changes and executes git status afterwards. |
| `g c` | commits. |
| `g ca` | commits after `g aa`. |
| `g cm "commits a commit lol"` | commits with a message. |
| `g cam "commits a commit lol"` | is `g ca` with a message. |
| TAGGING |
| `g tags` | is just another name for `git tag`, since it lists all tags, not only one. |
| `g tl` | `git tag`, which is same as `git tag --list` |
| `g ta` | `git tag --annotate`, opening an editor to add a tag with a (commit-like) message. |
| `g twm` | stands for `tag with message` and is an alias for `git tag --message='See CHANGELOG.md' --annotate` |
| `g td TAG` | `git tag --delete TAG` |
| MERGING |
| `g m BRANCH` | `git merge BRANCH` |
| `g squash BRANCH/COMMIT` | merges content without merging the git history. So the resulting commit looks like it has cherry-picked all commits of BRANCH/COMMIT. Very nice, if you want to keep some, but not all changes of a branch and merging this branch is not wished. |
| `g squeeze BRANCH/COMMIT` | is a different name for `g squash ...`. |
| BRANCHING |
| `g co BRANCH/COMMIT` | `git checkout BRANCH/COMMIT` |
| `g cob BRANCH` | `git checkout -b BRANCH` |
| `g b` | `git branch` |
| `g ba` | `git branch --list -a` |
| `g bv` | `git branch --list -v` |
| `g bav` | `git branch --list -av` |
| LOGGING |
| `g last N` | logs the last N commit messages. Default for N is 1. |
| `g l` | shows the history of currently HEADed commit as a graph. |
| `g la` | shows the global history as a graph. So it extends `g l` by, e.g., parallel histories. |

> __Note:__ `g l` uses `git log` and `g la` adds the flag `--all`.
> TLDR: With `--all`, also commits are shown, that are in parallel to the current HEAD.
> Due to `git help log`, this flag refers to stored references in `.git/refs`.


## Structure

In general, every file in `custom` can be removed and calling `configure` will recreate the default version of it.
So playing around with this project is only cricital in the case that own changes in `custom` are removed manually.
The idea behind the `custom`-folder is, besides supporting private files (e.g. ssh-configs), to reduce the need of forking/merging the project.

The following (incomplete) tree is supported after configuration.
Symlinks are marked with `@` at the end of their name and can be replaced manually for further customization.

```shell
dotfiles/
├── custom/
│   ├── custom/                         # completely untouched by the project
│   ├── git/
│   │   ├── config                      # includes ~/.gitconfig.general@
│   │   └── config.general@             # -> dotfiles/git/config.general
│   ├── shell/
│   │   ├── func/                       # extends/overrides shell/func/*
│   │   ├── ssh/
│   │   │   └── config
│   │   └── shellrc.sh                  # sources shell/shellrc.sh
│   └── vscode/
│       ├── keybindings.json@           # -> dotfiles/vscode/keybindings.json
│       └── settings.json@              # -> dotfiles/vscode/settings.json
├── git/
│   └── ...
├── kutgw/
│   └── ...
├── shell/
│   └── ...
├── utils/
│   └── ...
├── vscode/
│   ├── extensions.sh                   # install vscode-extensions
│   └── ...
└── README.md
```

|         folder in DOTFILES            | description |
|---------------------------------------|-------------|
| `custom/`                             | `dotfiles/custom/` is ignored by git (thus it could be set under git as well). It is organized exactly like the dotfiles-folder to use personal scripts replacing/extending default ones. The folder `dotfiles/custom/custom/` can be used to store general notes or thoughts, or to keep up good/old/outdated work. It won't be used in any script. |
| `custom/shell/ssh/config`             | `${HOME}/.ssh/config` symlinks to this. |
| `git/`                                | `git/config.general` contains useful git aliases and other settings. From `${HOME}`, `custom/git/config` and `custom/git/config.general` gets linked to. The file `git/config` is copied after initial configuration and may contain user specific info (e.g. `user.name`). |
| `kutgw/`                              | It stands for "keep up the good work". |
| `shell/`                              | `shell/` consists of scripts for setting the shell environment. `custom/shell/shellrc.sh` *fully* replaces `shell/shellrc.sh`. |
| `shell/func/`                         | provides useful shell functions. Functions in `custom/shell/func/` are autoloaded/included and overwrite default functions in `shell/func/` if their name is the same. |
| `shell/prompts/`                      | contains some prompts. |
| `utils/`                              | contains scripts for configuration. |
| `vscode/`                             | [Visual studio code](https://code.visualstudio.com/) uses some `settings.json` and `keybindings.json` for user settings and keybindings. |


## Contributing

These dotfiles are created in a __modular__ and __lightweight__ way.
For example, to find the `shellrc.sh`, the respective script is located in `shell`.

For more detailed information, please look [at the contribution section](CONTRIBUTING.md).


## FAQ / Troubleshooting

Weird experiences from friends and others, which are using this repo, are mentioned below.


### Syntax error

These dotfiles are used with `bash` and `zsh`.
Check how `sh` is symlinked, e.g. via

```shell
ls -1GF --color=auto -lh -a $(which sh)
```

For instance, `Debian` uses `dash` instead of `bash`.
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
