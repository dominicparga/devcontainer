# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog][keepachangelog], and this project adheres to [Semantic Versioning][semver].


## Table of contents

1. [Unreleased](#unreleased)
1. [v5.0.0](#v5.0.0)
1. [v4.0.0](#v4.0.0)
1. [v3.1.1](#v3.1.1)
    1. [v3.1.0](#v3.1.0)
    1. [v3.0.2](#v3.0.2)
        1. [v3.0.1](#v3.0.1)
        1. [v3.0.0](#v3.0.0)
1. [v2.0.1](#v2.0.1)
    1. [v2.0.0](#v2.0.0)
1. [v1.0.0](#v1.0.0)


## [Unreleased][github/self/unreleased] <a name="unreleased"></a>

### Added <a name="unreleased/added"></a>

\-


### Changed <a name="unreleased/changed"></a>

\-


### Deprecated <a name="unreleased/deprecated"></a>

\-


### Removed <a name="unreleased/removed"></a>

\-


### Fixed <a name="unreleased/fixed"></a>

\-


### Security <a name="unreleased/security"></a>

\-


## [v5.0.0][github/self/v5.0.0] <a name="v5.0.0"></a>

### Added <a name="v5.0.0/added"></a>

- Support `R`.
- When pulling, forbid `rebase` in `config.general`.
- Add shell-function `grep_find` for using `grep` in combination with `find`.


### Changed <a name="v5.0.0/changed"></a>

- Update `README.md`.
- Update `alacritty.yml` after updating `alacritty`.
- Update vscode-extensions and vscode-settings.


### Deprecated <a name="v5.0.0/deprecated"></a>

- Add github-actions.
- Move issue-content to `notes.md`.


### Removed <a name="v5.0.0/removed"></a>

- Remove code referring to `macOS` since it is unused for months.
- Cleanup `kutgw/`
  - Remove `neovim`-configs since it is unused for years.
  - Remove some functions for creating projects.


### Fixed <a name="v5.0.0/fixed"></a>

- Fix setting `$DOTFILES` in `configure`
- Fix `alias ...=cd ../../..` to `alias ...=cd ../..` (and others)


## [v4.0.0][github/self/v4.0.0] <a name="v4.0.0"></a>

### Added <a name="v4.0.0/added"></a>

- Add `CHANGELOG.md`.
- Add file containing __version__ for github-actions and automatic tagging.
- Add __gitconfig__-shortcuts for tagging.
- Add __`notes.md`__ for adding random stuff, like links and new info.
- Add __terminal-keybindings__ for uniform and controlled usage in different terminals (treated as breaking change, since not tested with every existing terminal).
- Add new __terminal-alias__ for rebooting the system (`reboot='shutdown now --reboot`).
- Add __vscode-settings__
  - Remember more lines for scrolling back in __integrated terminal__.
  - Continue setting up __`latexmk`__.
  - Add __markdown__-rules.


### Changed <a name="v4.0.0/changed"></a>

- Let alias for `tree` print the element-type (like `dir -> dir/`)


### Deprecated <a name="v4.0.0/deprecated"></a>

- Fill `CHANGELOG.md`
- Replace existing tags with empty ones and add text to the `CHANGELOG.md`
- Add github-actions.
- Move issue-content to `notes.md`.


### Removed <a name="v4.0.0/removed"></a>

- Section `News` from `README.md`.


## [v3.1.1][github/self/v3.1.1] <a name="v1.0.0"></a>

### Deprecated <a name="v3.1.1/deprecated"></a>

- todo
- `README.md` has ugly text in Usage-table..


## [v3.1.0][github/self/v3.1.0] <a name="v1.0.0"></a>

### Deprecated <a name="v3.1.0/deprecated"></a>

- todo


## [v3.0.2][github/self/v3.0.2] <a name="v1.0.0"></a>

### Deprecated <a name="v3.0.2/deprecated"></a>

- todo


## [v3.0.1][github/self/v3.0.1] <a name="v1.0.0"></a>

### Deprecated <a name="v3.0.1/deprecated"></a>

- todo


## [v3.0.0][github/self/v3.0.0] <a name="v1.0.0"></a>

### Deprecated <a name="v3.0.0/deprecated"></a>

- todo
  - From section `News` from `README.md`:
    - Version `3.0.0` has been finished.
      It refactors the shellscripts to be cleaner and POSIX-conform (where possible) thanks to the shell-linter `shellcheck`.



## [v2.0.1][github/self/v2.0.1] <a name="v1.0.0"></a>

### Deprecated <a name="v2.0.1/deprecated"></a>

- todo


## [v2.0.0][github/self/v2.0.0] <a name="v1.0.0"></a>

### Deprecated <a name="v2.0.0/deprecated"></a>

- todo
  - From section `News` from `README.md`:
    - Version `2.0.0` has been finished.
      It moves boilerplate-code (for installation) to [the howto-repo][web_github_howto].
      This allows a more flat and clean code-structure.
      Due to semantic versoning, the new version implies a breaking change.
      This affects symlinks in `${HOME}/` and `${DOTFILES}/custom/`.

      In general, automatic tests (and a Docker-Image?) could help with testing.
      A changelog could be helpful as well.


## [v1.0.0][github/self/v1.0.0] <a name="v1.0.0"></a>

### Deprecated <a name="v1.0.0/deprecated"></a>

- todo


[keepachangelog]: https://keepachangelog.com/en/
[semver]: https://semver.org/

[github/self/unreleased]: https://github.com/dominicparga/dotfiles/compare/v5.0.0...HEAD
[github/self/v5.0.0]: https://github.com/dominicparga/dotfiles/compare/v4.0.0...v5.0.0
[github/self/v4.0.0]: https://github.com/dominicparga/dotfiles/compare/v3.1.1...v4.0.0
[github/self/v3.1.1]: https://github.com/dominicparga/dotfiles/compare/v3.1.0...v3.1.1
[github/self/v3.1.0]: https://github.com/dominicparga/dotfiles/compare/v3.0.2...v3.1.0
[github/self/v3.0.2]: https://github.com/dominicparga/dotfiles/compare/v3.0.1...v3.0.2
[github/self/v3.0.1]: https://github.com/dominicparga/dotfiles/compare/v3.0.0...v3.0.1
[github/self/v3.0.0]: https://github.com/dominicparga/dotfiles/compare/v2.0.1...v3.0.0
[github/self/v2.0.1]: https://github.com/dominicparga/dotfiles/compare/v2.0.0...v2.0.1
[github/self/v2.0.0]: https://github.com/dominicparga/dotfiles/compare/v1.0.0...v2.0.0
[github/self/v1.0.0]: https://github.com/dominicparga/dotfiles/releases/tag/v1.0.0
