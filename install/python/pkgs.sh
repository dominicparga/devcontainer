_pip_packages=(
    pipenv
    autopep8
    docopt
    pylint
)
for _item in ${_pip_packages[@]}; do
    pip3 install --user ${_item}
    pip2 install --user ${_item}
    echo
done
