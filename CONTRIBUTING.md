# Contributing to these dotfiles

The following is a set of (style) guidelines for contributing to my dotfiles.
Feel free to propose changes to this document in a pull request.

## Table of Contents

* [Code of Conduct](#code-of-conduct)
* [Git Commit Messages](#git-commit-messages)
* [Gitflow Workflow](#gitflow-workflow)
* [Releases](#releases)
  * [Release Checklist](#release-checklist)
  * [Semantic Versioning](#semantic-versioning)
* [Shell](#shell)
  * [File Style](#file-style)
  * [Coding Conventions](#coding-conventions)
  * [Documentation](#documentation)
  * [Project Conventions](#project-conventions)

---

## Code of Conduct

This project and everyone participating in it is governed by the [Code of Conduct](CODE_OF_CONDUCT.md).
By participating, you are expected to uphold this code.

## [Git Commit Messages][website_git_commit_messages]

Commit messages are a _hugely_ important part of working with git.
They not only help other workers to understand changes quickly, but also are the basement for new releases and their release notes.
Thus, these following rules [cited from here][website_git_commit_messages] should be accounted in commits.

First of all, commits should be `atomic`.
One commit should contain only changes of a few lines of code (or one single feature).
This method seems to be too verbose and kind of annoying, but when working with Git logs (`git log`) or GitHub's network tree, this is a huge advantage.
Branch management, releases and especially finding bugs is way easier with small commit messages.

In summary, a properly formed Git commit subject line should always be able to complete the following sentence:  
  `This commit <subject line>`

Some points about the commit message style:

* Separate `subject` from `body` with a blank line.
  The body explains _what_ has changed and _why_, not _how_ it has changed.
  _How_ can be checked by looking at the commit changes itself.
* Line widths
  * 1st line (`subject`) up to 50 characters
  * 2nd line empty
  * Remaining (`body`) lines up to 72 characters
* Lowercase the `subject` line

  ```diff
  - Fixes typo ...
  + fixes typo ...
  ```

* Do not end the `subject` line with a period

  ```diff
  - refactor brackets of some if-statements.
  + refactor brackets of some if-statements
  ```

* Use the present tense  

  ```diff
  - added feature
  + adds feature
  ```

* Use the 3rd person singular mood, no other language styles (no description either)  

  ```diff
  - move cursor to ...
  - fixed bug ...
  - sweet new API methods ...
  + moves cursor to ...
  + fixes bug ...
  + adds new API methods for ...
  ```

Consider using following verbs/flags in commit messages:

* `fix` when the commit contains bug fixes
* `doc(s)` when writing documentation
* `test(s)` when tests were changed/added
* `style` when code or format style changes occur in the commit
* `refactor` when changes __DO NOT__ change functionality

## [Gitflow Workflow][website_gitflow_workflow]

This project uses [Gitflow Workflow][website_gitflow_workflow], a nice and clear way to work effectively.

This means following branches are used:

* `master`: the official release (using tags)
* `develop`: branch for active development
* `release/<tag>`: temporary branch off `develop` for bug fixes and docs before getting merged into `master`
* `feature/<name>`: branches for specific feature development
* `hotfix/<name>`: branches for bug fixes branched off `master`
* `fix/<name>`: branches for bug fixes branched off `develop`

## Releases

The following template for new releases can be used.
In general, needed information should be optained from commit messages.

```text
## General info

<optional>


## New features

<Short and interesting description about new features of this release.>


## Bug fixes

<Detailed description about fixed bugs of this release.>
```

### Release Checklist

These points should be considered and checked for a new release.

* Has `release` been merged into `master`?
* Is a new teaser picture needed/recommended?

### [Semantic Versioning][website_semantic_versioning]

"Given a version number `MAJOR.MINOR.PATCH`, increment the:

* `MAJOR` version when incompatible API changes are made,
* `MINOR` version when functionality in a backwards-compatible manner is added, and
* `PATCH` version when backwards-compatible bug fixes are made.

Exception could be alpha or beta status, when the software hasn't reached 1.0.0 yet.
Additional labels for pre-release and build metadata are available as extensions to the `MAJOR.MINOR.PATCH`."

For more information, see [the original site][website_semantic_versioning].

## Shell

The following conventions and suggestions should be followed.
They help a lot keeping overview and the code clear.

Although the language is bash or zsh, the project's style bases on python's philosophie.

[`The Zen of Python`][website_wikipedia_zen_of_python] should be kept in mind while coding.
From [Wikipedia - Python][website_wikipedia_python]:  
In general, the language's core philosophy (...) includes

```text
Beautiful is better than ugly.

Explicit is better than implicit.

Simple is better than complex.

Complex is better than complicated.

Readability counts.
```

which may be called `pythonic`.

### File Style

* Maximum line width is `80`.

  _Humans have trouble reading the code with increasing line width.
  In general, more than `80` is not recommended._

* Use `4 spaces` for indention (p.s.: [could help salary](https://stackoverflow.blog/2017/06/15/developers-use-spaces-make-money-use-tabs)!).

### Coding Conventions

* Functions, fields and file names are written in `snake_case`.

  Since functions and fields are added to the shell's environment, their names are slightly changed to prevent conflicts or verbose autocompletion and help understanding the code:
  * private fields' names begin with a __single underscore__
  * exported ("public") fields' names are written in __CAPSLOCK__
  * private functions' names begin with __double underscore__
  * public functions' names doesn't begin with any underscore

  ```zsh
  # BAD
  Greet() {
      used_shell="unknown"
      # ...
  }

  export dotfiles="..."



  # GOOD
  # used in the shell, hence no leading dunderscore
  greet() {
      local _used_shell="unknown"
      if [[ -n "${ZSH_NAME}" ]]; then _used_shell="zsh"; fi
      if [[ -n "${BASH}" ]];     then _used_shell="bash"; fi

      echo ""
      echo "Hello ${USER}."
      echo "I'm a ${_used_shell} window at your service."
      echo ""
  }

  export DOTFILES="..."
  ```

* Always use `${VAR}` over `$VAR` for sake of consistency.

  _It helps a lot in doing automatic stuff like replacing code in an automatic way (e.g. using vi)._

  ```diff
  - $VAR
  - $1
  + ${VAR}
  + ${1}
  ```

* Use `constants` over `magic numbers` (or often used paths)!

  _Even the author will not know the meaning of every number after several months.
  And if he knows though, he will probably forget the precision of the constant and the places, where he has put them (-> bad for debugging)._

  ```zsh
  # BAD
  # because the path is often used

  if [[ -d "<path to shell lib>/func" ]]; then
      ...
  fi

  if [[ -d "<path to shell lib>/prompts" ]]; then
      ...
  fi



  # GOOD

  _shell_lib="<path to shell lib>"
  if [[ -d "${_shell_lib}/func" ]]; then
      ...
  fi

  if [[ -d "${_shell_lib}/prompts" ]]; then
      ...
  fi
  ```

* Make __visibility as closest__ as possible.

  _Usually, programmer tend to not bother with visibility, but visibility helps a lot with getting nice and persistent interfaces._

  In shell, other scripts can be sourced, which acts like adding the code of the called script inside the calling script.
  Defined functions and fields are visible in the shells's environment.
  To remove them, call `unset <name>`.

* Use __white spaces around binary operators__.
  Per default, shell scripting is able to execute integer calculations, e.g. `echo $((3 + 4))`.
  Exceptions can be made for special cases to improve readability (see below).

  ```diff
  - e = (- a) * b;
  + e = (-a) * b;

  - e = a*b;
  + e = a * b;

  + e = a * b + c * d;    # ok, but not recommended here
  + e = a*b + c*d;        # improves readability
  ```

* Single quotes `'` preserve the literal value of each character, double quotes `"` interprete some of them.

  ```zsh
  $ echo "$(echo "hello")"
  hello

  $ echo '$(echo "hello")'
  $(echo "hello")
  ```

* `[[ ... ]]` vs `[ ... ]` vs `((...))` vs `(...)` vs `{...}`  
  See this super [stackoverflow answers][stackoverflow_brackets].
  In a nutshell:
  * `[[ ... ]]` has additional features over `[ ... ]` and is preferred.
  * `(...)` executes shell commands and influences execution order.
  * `((...))` executes arithmetic instructions (no floats)
  * `{...}` groups commands which only influences parsing, so it doesn't change the execution order. Example:  
    `x=2; { x=4; }; echo ${x}` prints `4`  
    `x=2; ( x=4; ); echo ${x}` prints `2`

### Documentation

* Separate module sections with `#######` (whole line).
  Take the following code snippet for inspiration.

  ```python
  ##########################################################
  # exports

  . "${_shell_lib}/exports.sh"
  if [[ -f "${_custom_shell_lib}/exports.sh" ]]; then
      . "${_custom_shell_lib}/exports.sh"
  fi

  ##########################################################
  # aliases

  . "${_shell_lib}/aliases.sh"
  if [[ -f "${_custom_shell_lib}/aliases.sh" ]]; then
      . "${_custom_shell_lib}/aliases.sh"
  fi
  ```

### Project Conventions

* Prefer package/folder management over file mangement if `meaningful`.  
  __BUT:__ Think in an intuitive, handy and `deterministic`(!) way and don't take structuring and subfolding too far.

  Always ask yourself:  
  `How would most of the people search for this module/class/file?`  
  Someone without knowing the whole project structure should be able to find a file at the first try.  
  `In every folder, there should be only one option to continue searching (-> determinism).`

---

Back to [Table of Contents][website_contributing_toc]

[website_contributing_toc]: #table-of-contents

[website_git_commit_messages]: https://chris.beams.io/posts/git-commit
[website_gitflow_workflow]: https://www.atlassian.com/git/tutorials/comparing-workflows/gitflow-workflow
[website_semantic_versioning]: https://semver.org

[website_wikipedia_zen_of_python]: https://en.wikipedia.org/wiki/Zen_of_Python
[website_wikipedia_python]: https://en.wikipedia.org/wiki/Python_(programming_language)#Features_and_philosophy

[stackoverflow_brackets]: https://unix.stackexchange.com/questions/306111/what-is-the-difference-between-the-bash-operators-vs-vs-vs
