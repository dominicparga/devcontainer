# Submodule

## Removing a submodule

Quelle:
[stackoverflow](https://stackoverflow.com/questions/1260748/how-do-i-remove-a-submodule)

To remove a submodule you need to:

1. Delete the relevant section from the .gitmodules file.
2. Stage the .gitmodules changes `git add .gitmodules`
3. Delete the relevant section from .git/config.
4. Run `git rm --cached 'path/to/submodule'` (no trailing slash).
5. Run `rm -rf .git/modules/path/to/submodule`
6. Commit `git commit -m "Removed submodule 'name'"`
7. Delete the now untracked submodule files
8. `rm -rf 'path/to/submodule'`

## More sources

- [git doc](https://git-scm.com/book/de/v1/Git-Tools-Submodule)
- [cheatsheet](https://www.systutorials.com/5520/git-submodule-cheat-sheet/)
