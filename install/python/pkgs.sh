_pip_packages=(
    pipenv
    autopep8
    docopt
    pylint
    setuptools
    pip
    numpy
    matplotlib
)
for _item in ${_pip_packages[@]}; do
    python3 -m pip install -U ${_item}
    python2 -m pip install -U ${_item}
    echo
done
