# devcontainer

[![License](https://img.shields.io/github/license/dominicparga/devcontainer?style=for-the-badge)](https://github.com/dominicparga/devcontainer/blob/stable/LICENSE)
[![Tags](https://img.shields.io/docker/v/dominicparga/devcontainer?label=Latest%20tag&logo=docker&style=for-the-badge)](https://hub.docker.com/r/dominicparga/devcontainer)
[![Last commit](https://img.shields.io/github/last-commit/dominicparga/devcontainer?label=Latest%20tag&logo=github&style=for-the-badge)](https://github.com/dominicparga/devcontainer/commits)

## Usage

This is my personal VSCode devcontainer.
It contains some specific user settings.
Thus I'd appreciate not using my image+`Dockerfile` without respective adjustments.

## FAQ / Troubleshooting

Weird experiences from friends and others, which are using this repo, are mentioned below.

### Syntax error

These devcontainer is used with `bash`.
Check how `sh` is symlinked, e.g. via

```shell
ls -1GF --color=auto -lh -a $(which sh)
```

For instance, `Debian` uses `dash` instead of `bash` per default.
In this case, you can change the used shell in your terminal.
However, probably the best option is changing the default system shell with `chsh`.
