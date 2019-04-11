# dotfiles

Dry and short:  
These dotfiles should speed up a personal workflow and help installing and setting up a personal working system.
Focus lays on keeping overview since using this repo should feel like speeding up manual steps.
Therefore, kind of plugin-structure is not implemented.
Instead, (more or less) slight customization of existing files is supported providing some flexibility without the need of forking the whole project.

Hence, the general idea of these files is:  

1. Create a `custom` folder inside the folder `dotfiles` (ignored by git).
2. Create all necessary dotfiles in `custom`.
3. Include and source files of this project.
4. Create symlinks from `$HOME` linking to files in `custom folder`.

All these steps can be done automatically by using the provided function `dotfiles`.

## Features

Besides the function `dotfiles` for handy setup, useful aliases and functions are provided.
These dotfiles are used with `bash` and `zsh`.

### Shell environment

Beside autocompletion for several tools, aliases and exports can be found in [`shell/shellrc.sh`](https://github.com/dominicparga/dotfiles/blob/master/shell/shellrc.sh).
Some commands like `grep` or `tree` are just flagged to use colors.
Those are not mentioned here.

| new cmd | note |
|:---------:|------|
| SAFETY ALIASES |
| <kbd>cp</kbd> | is alias for `cp -i -P`, so asks before replacing existing file. |
| <kbd>mv</kbd> | is alias for `mv -i`, so asks before replacing existing file. |
| COMMAND ls |
| <kbd>l</kbd> | shows items as a colored list. |
| <kbd>la</kbd> | shows hidden items as well. |
| <kbd>ll</kbd> | shows items as a colored list and access rights. |
| <kbd>lla</kbd> | shows hidden items as well. |
| WORKING QUICKLY |
| <kbd>c</kbd> | is alias for `clear`. |
| <kbd>g</kbd> | is alias for `git`. |
| <kbd>..</kbd> | is alias for `cd ..`. |
| <kbd>.2</kbd> or <kbd>...</kbd> | goes back 2 folders. |
| <kbd>.3</kbd> or <kbd>....</kbd> | goes back 3 folders. |
| ... | ... |
| <kbd>.6</kbd> or <kbd>.......</kbd> | goes back 6 folders. |
| <kbd>mkd</kbd> | creates folder(s) and enters it. |
| MORE HANDY FUNCTIONS |
| <kbd>alert</kbd> | can be called like `sleep 2; alert`. |
| <kbd>dotfiles</kbd> | for setting scripts and installing tools. |
| <kbd>gitignore</kbd> | uses the `gitignore.io` API to return gitignore entries. |

### Git aliases

Have a look at the handy [git aliases](https://github.com/dominicparga/dotfiles/blob/master/git/config).
In addition, visual-studio-code is opening as diff-tool and for commit-messages.

### System setup

You can setup your system (macOS, linux) using `dotfiles install-system`.
Executing `dotfiles help` may help.

## Structure

The following (incomplete) tree is supported.
In general, every file in custom can be removed and calling `dotfiles custom` will recreate the default version of it.
So playing around with this project is only cricital in the case that own changes in custom are removed manually. ;)

The idea behind the custom folder is, besides supporting private files (e.g. ssh-configs), to reduce the need of forking/merging the project.
Every symlink will link to a file in custom, that usually executes the draft outside the custom folder per default.

```zsh
dotfiles/
├── custom/
│   ├── custom/                         # completely untouched by the project
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
dotfiles symlink all
```

*__NOTE:__ `custom/` contains some generic info that should probably be updated by hand (e.g. gitconfig's `user.name`).*

### Optional: Change default location

Changing the default location `${HOME}/dotfiles` needs to rename the folder and change the variable `DOTFILES` defined at top of `${DOTFILES}/shell/shellrc.sh`.
To provide this without the need of forking the whole project, a file `custom/shell/shellrc.sh` is preferred over the default `shell/shellrc.sh`.

`dotfiles symlinks` (respectively `dotfiles custom`) creates the file `custom/shell/shellrc.sh` (amongst others), but you have to remove it manually before.

```zsh
# e.g. choosing .dotfiles instead of dotfiles as folder name
export DOTFILES="${HOME}/.dotfiles"

cd ~
git clone https://github.com/dominicparga/dotfiles.git "${DOTFILES}"

. "${DOTFILES}/shell/shellrc.sh"
dotfiles symlink all
```

### Optional: Manual setup

The wrapper function calls the following scripts.

```zsh
cd ~
git clone https://github.com/dominicparga/dotfiles.git "dotfiles"

. "${HOME}/dotfiles/shell/shellrc.sh"
bash "${DOTFILES}/utils/create_custom.sh"
bash "${DOTFILES}/utils/symlink_dotfiles.sh" all
```

Fully manually, `custom/` and all symlinks has to be created by hand.

## Contributing

These dotfiles are created in a __modular__ and __lightweight__ way.
For example, to find the `shellrc.sh`, the respective script is located in `shell`.
This should be kept (in general) since looking for, e.g., "python" should not need you to look in other folders than `python/`.

For more detailed information, please look [at the contribution section](CONTRIBUTING.md).

## Troubleshooting

If Ubuntu doesn't run the scripts as expected, check how `sh` is linked.
These dotfiles are used with bash and zsh.
For instance, `dash` does not support `[[ ... ]]`, which is used a lot here.

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

- alert: add usage description to alert function (sound arg)
- alert: add message to alert as input arg
- dotfiles: print info of all features and new functionality
- editing: function for quickly editing shellrc (other files?)
- java: function for changing java version
- kubectl: probably too slow like <(heroku ...)?
- LaTeX: script for creating a LaTeX folder structure
- LaTeX: install
- macOS: install Nord and Dracula
- prompt: function for changing prompt
- python: pip install $(pip list --outdated | awk '{ print $1 }') --upgrade
- README: add git shortcuts
- README: add vscode (vim-)keybindings
- ubuntu: install Dracula
- ubuntu: set PYTHON_INTERPRETER_PATH in shellrc
- vscode: cool keybindings
