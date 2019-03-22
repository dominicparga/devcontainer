################################################################################
# update apt first

sudo -v
sudo apt-get update -y
sudo apt-get upgrade -y

################################################################################
# define and install packages

_packages=(
    neovim
    tree
    python3
    python
    zsh
    openjdk-11-jdk
    scala
)

for _item in ${_packages[@]}; do
    sudo -v
    sudo apt-get install -y ${_item}
    echo
done
