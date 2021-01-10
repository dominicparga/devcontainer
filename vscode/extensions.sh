#!/usr/bin/env bash

#------------------------------------------------------------------------------#
## fill in custom extensions here
## '--force' for updating
## 'code --disable-extensions' would disable installed extensions

## color and icon themes
code --force --install-extension arcticicestudio.nord-visual-studio-code
code --force --install-extension be5invis.vscode-icontheme-nomo-dark
code --force --install-extension dracula-theme.theme-dracula
code --force --install-extension laurenttreguier.vscode-simple-icons
code --force --install-extension monokai.theme-monokai-pro-vscode
code --force --install-extension pkief.material-icon-theme
code --force --install-extension vscode-icons-team.vscode-icons
code --force --install-extension alexesprit.vscode-unfancy-file-icons

## editor utils
code --force --install-extension coenraads.bracket-pair-colorizer-2
code --force --install-extension davidhouchin.whitespace-plus
code --force --install-extension pnp.polacode
code --force --install-extension vscodevim.vim

## git
code --force --install-extension donjayamanne.githistory

## markdown
code --force --install-extension davidanson.vscode-markdownlint

## toml
code --force --install-extension bungcip.better-toml
## csv
code --force --install-extension mechatroner.rainbow-csv
## yaml
#code --force --install-extension redhat.vscode-yaml
## xml
code --force --install-extension redhat.vscode-xml
## postgresql
#code --force --install-extension ckolkman.vscode-postgres

## shell
code --force --install-extension timonwong.shellcheck

## rust
## rust-lang.rust xor matklad.rust-analyzer
#code --force --install-extension rust-lang.rust
#code --force --install-extension matklad.rust-analyzer
#code --force --install-extension a5huynh.vscode-ron # like json, for rust

## javascript/markdown
#code --force --install-extension esbenp.prettier-vscode
#code --force --install-extension dbaeumer.vscode-eslint

## python
#code --force --install-extension kevinrose.vsc-python-indent
#code --force --install-extension ms-python.python

## R
#code --force --install-extension ikuyadeu.r
#code --force --install-extension reditorsupport.r-lsp
## after setting-up R, e.g. .Rprofile and .Renviron, try:
## Rscript -e 'install.packages("languageserver")'

## LaTeX
#code --force --install-extension james-yu.latex-workshop
## August 2020: doesn't support cite-autocompletion :(
## code --force --install-extension efoerster.texlab

## c cpp
#code --force --install-extension cquera-project.cquery
#code --force --install-extension ms-vscode.cpptools
#code --force --install-extension twxs.cmake
#code --force --install-extension vector-of-bool.cmake-tools

## java
#code --force --install-extension redhat.java
#code --force --install-extension vscjava.vscode-java-debug
#code --force --install-extension vscjava.vscode-java-pack
#code --force --install-extension vscjava.vscode-java-test

## gradle
#code --force --install-extension naco-siren.gradle-language

## maven
#code --force --install-extension vscjava.vscode-maven
