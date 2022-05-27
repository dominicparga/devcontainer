# Shell environment

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
