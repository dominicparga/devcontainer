# Git aliases

Have a look at the handy [git aliases](src/home/.gitconfig).
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
