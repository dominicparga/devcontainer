################################################################################
# fill in custom extensions here
# in ALPHABETICAL order

_extensions=(
    # color and icon themes
    arcticicestudio.nord-visual-studio-code
    be5invis.vscode-icontheme-nomo-dark
    dracula-theme.theme-dracula
    laurenttreguier.vscode-simple-icons
    monokai.theme-monokai-pro-vscode
    pkief.material-icon-theme
    robertohuertasm.vscode-icons

    # editor utils
    coenraads.bracket-pair-colorizer-2
    davidhouchin.whitespace-plus
    # minhthai.vscode-todo-parser
    pnp.polacode
    # slevesque.vscode-hexdump
    vscodevim.vim
    # wayou.vscode-todo-highlight

    # git
    donjayamanne.githistory
    # waderyan.gitblame

    # markdown
    davidanson.vscode-markdownlint

    # toml
    bungcip.better-toml
    # csv
    mechatroner.rainbow-csv

    # rust
    rust-lang.rust

    # python
    kevinrose.vsc-python-indent
    ms-python.python

    # c cpp
    # cquera-project.cquery
    ms-vscode.cpptools
    # twxs.cmake
    # vector-of-bool.cmake-tools

    # java
    redhat.java
    vscjava.vscode-java-debug
    vscjava.vscode-java-pack
    vscjava.vscode-java-test

    # scala
    daltonjorge.scala

    # gradle
    # naco-siren.gradle-language

    # maven
    vscjava.vscode-maven
)

################################################################################
# install extensions

for _item in ${_extensions[@]}; do
    # force for updating
    code --force --install-extension ${_item}
done
#code --disable-extensions
