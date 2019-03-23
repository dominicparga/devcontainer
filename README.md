# dotfiles

Dry and short:  
These dotfiles should speed up a personal workflow and help installing and setting up a personal working system.
Focus lays on keeping overview since using this repo should feel like speeding up manual steps.
Therefore, kind of plugin-structure isn't realized.
Instead, (more or less) slight customization of existing files is supported providing some flexibility without the need of forking the whole project.

## Usage

The project has to be cloned to `${HOME}/dotfiles` and the provided wrapper function can be used to set all symlinks.
Executing the following will create a folder `custom/...` in the dotfiles folder and create all needed symlinks in HOME.

*__NOTE:__ This default location can be changed.
See [Change default location](#change-default-location) below for more infos.
The function name `dotfiles` is independent of your chosen foldername.*

```zsh
cd ~
git clone https://github.com/dominicparga/dotfiles.git

. "${HOME}/dotfiles/shell/shellrc.sh"
dotfiles symlinks
```

`custom/` contains some generic info that should probably be updated by hand (e.g. gitconfg's `user.name`).

### Change default location

Changing the default location `${HOME}/dotfiles` needs to rename the folder and change the variable `DOTFILES` defined at top of `${DOTFILES}/shell/shellrc.sh`.
To provide this without the need of forking the whole project, a file `custom/shell/shellrc.sh` is preferred over the default `shell/shellrc.sh`.

`dotfiles symlinks` (respectively `dotfiles custom`) creates the file `custom/shell/shellrc.sh` (amongst others), but you have to remove it manually before.

```zsh
# choosing .dotfiles instead of dotfiles as folder name
export DOTFILES="${HOME}/.dotfiles"

cd ~
git clone https://github.com/dominicparga/dotfiles.git "${DOTFILES}"

. "${DOTFILES}/shell/shellrc.sh"
dotfiles symlinks
```

### Manual setup

The wrapper function calls the following scripts.

```zsh
cd ~
git clone https://github.com/dominicparga/dotfiles.git "dotfiles"

. "${HOME}/dotfiles/shell/shellrc.sh"
sh "${DOTFILES}/utils/create_custom.sh"
sh "${DOTFILES}/utils/symlink_dotfiles.sh"
```

Fully manually, `custom/` and all symlinks has to be created by hand.

## Contributing

These dotfiles are created in a __modular__ and __lightweight__ way.
For example, to find the `shellrc.sh`, the respective script is located in `shell`.
This should be kept (in general) since looking for, e.g., "python" should not need you to look in other folders than `python/`.

For more detailed information, please look [at the contribution section](CONTRIBUTING.md).

## Folder structure

The following (incomplete) tree is supported.
In general, every file in custom can be removed and calling `dotfiles custom` will recreate the default version of it.

```zsh
dotfiles/
├── custom/
│   ├── custom/
│   ├── git/
│   │   └── config                      # includes git/config per default
│   ├── install/
│   │   ├── macOS/
│   │   ├── python/
│   │   ├── ubuntu/
│   │   └── vscode/
│   ├── shell/
│   │   ├── func/                       # extends/overrides shell/func/*
│   │   ├── ssh/
│   │   │   └── config                  # symlinked to
│   │   └── shellrc.sh                  # sources shell/shellrc.sh
│   └── vscode/
│       ├── keybindings.json            # symlinked to
│       └── settings.json               # symlinked to
├── git/
├── install/
├── kutgw/
├── macOS/
├── shell/
├── utils/
│   └── drafts/                         # defaults for custom
└── README.md
```

|         folder in DOTFILES            | description |
|---------------------------------------|-------------|
| `custom/`                             | `dotfiles/custom/` is ignored by git (thus it could be set under git as well). It is organized exactly like the dotfiles-folder to use personal scripts replacing/extending default ones. The folder `dotfiles/custom/custom/` can be used to store general notes or thoughts, or to keep up good/old/outdated work. It won't be used in any script. |
| `custom/shell/ssh/config`             | `${HOME}/.ssh/config` symlinks to this. |
| `git/`                                | `git/config` contains useful git aliases and other configs. From HOME, `custom/git/config` gets linked to. It includes `git/config` and may contain user specific info (e.g. `user.name`). |
| `install/`                            | Scripts like `install/python/pkgs.sh` helps setting up a system. When using the dotfiles' wrapper function, custom counterparts of them are preferred and replaces the default ones. |
| `kutgw/`                              | It stands for "keep up the good work". |
| `shell/`                              | `shell/` consists of scripts for setting the shell environment. `custom/shell/shellrc.sh` *fully* overwrites `shell/shellrc.sh`. |
| `shell/func/`                         | provides useful shell functions. Functions in `custom/shell/func/` are autoloaded/included and overwrite default functions in `shell/func/` if their name is the same. |
| `shell/prompts/`                      | contains some prompts. |
| `utils/`                              | contains scripts for interacting with the dotfiles quickly. |
| `utils/drafts/` | Contains drafts that will be copied into a fresh created `custom/`. It is the solution to the problem of having (frequently changing) user dependent scripts (e.g. vscode's `settings.json`) and a git repo, that shouldn't need to be forked only for slight changes. |
| `vscode/`                             | [Visual studio code](https://code.visualstudio.com/) uses some `settings.json` and `keybindings.json` for user settings and keybindings. Since these files slightly change a lot in usage, often temporary, they are provided as drafts and symlinked to `custom/vscode/...`. |

## Features

Per default, useful aliases are provided.

### Shell aliases

Please refer to [`shell/aliases.sh`](https://github.com/dominicparga/dotfiles/blob/master/shell/aliases.sh).

### Git aliases

Have a look at the great [`git aliases`](https://github.com/dominicparga/dotfiles/blob/master/git/config).
Besides obvious ones, there are as well:

```zsh
# g is alias for git

# shows last <N> (default: 3) logs
g last <N>

# shows global branch history
g hist
g graph

# shows HEAD history
g localhist
g localgraph
```

In addition, visual-studio-code is opening as diff-tool and for commit-messages.

### System setup

You can setup your system (macOS, linux) using `dotfiles install-system`.

Executing `dotfiles help` may help.

## TODO

### add notes

- installation notes
  - download nord
  - install xcode-tools
  - hidden files
  - virtualbox needs sth.
  - teamviewer settings
  - magnet aus Apple Store
- refactor 3-lines

### shell scripting

- dotfiles: add function for quickly editing shellrc (other files?)
- macOS: bash autocompletion (since macOS has bash 3.2 ._.)
- java: change JAVA_HOME
- add shebangs to executable files
- macOS: install Nord and Dracula
- ubuntu: install Dracula
- ubuntu: implement alert function
- add usage description to alert function (sound arg)
- add message to alert as input arg
- macOS: install LaTeX
- ubuntu: install LaTeX
- script for creating a LaTeX folder structure
