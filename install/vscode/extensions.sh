################################################################################
# fill in custom extensions here
# in ALPHABETICAL order

_extensions=(
    arcticicestudio.nord-visual-studio-code
    be5invis.vscode-icontheme-nomo-dark
    coenraads.bracket-pair-colorizer-2
#    cquera-project.cquery
    daltonjorge.scala
    davidanson.vscode-markdownlint
    davidhouchin.whitespace-plus
    donjayamanne.githistory
    dracula-theme.theme-dracula
    laurenttreguier.vscode-simple-icons
    mechatroner.rainbow-csv
#    minhthai.vscode-todo-parser
    monokai.theme-monokai-pro-vscode
    ms-python.python
    ms-vscode.cpptools
#    naco-siren.gradle-language
    pkief.material-icon-theme
    pnp.polacode
    redhat.java
    robertohuertasm.vscode-icons
#    twxs.cmake
#    vector-of-bool.cmake-tools
    vscjava.vscode-java-debug
    vscjava.vscode-java-pack
    vscjava.vscode-java-test
    vscjava.vscode-maven
    vscodevim.vim
#    wayou.vscode-todo-highlight
)

################################################################################
# install extensions

for _item in ${_extensions[@]}; do
    # force for updating
    code --force --install-extension ${_item}
done
#code --disable-extensions
