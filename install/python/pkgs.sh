_pip_packages=(
    autopep8
    docopt
    matplotlib
    numpy
    pip
    pipenv
    pylint
    setuptools
)
for _item in ${_pip_packages[@]}; do
    python3 -m pip install -U ${_item}
    python2 -m pip install -U ${_item}
    echo
done
