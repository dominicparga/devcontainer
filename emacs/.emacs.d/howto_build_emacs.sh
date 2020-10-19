#!/bin/sh
# Howto build emacs

# install dependencies
sudo apt-get install -y \
     build-essential \
     git autoconf \
     texinfo \
     libgnutls28-dev \
     libxml2-dev \
     libncurses5-dev \
     libjansson-dev

# clone and configure repo
VERSION="27.1"
SRC_PATH="${HOME}/opt/emacs-${VERSION}"
# git clone git://git.sv.gnu.org/emacs.git "${SRC_PATH}"
cd "${SRC_PATH}" || exit
./autogen.sh
CFLAGS="-ggdb3 -O0" CXXFLAGS="-ggdb3 -O0" LDFLAGS="-ggdb3" ./configure --with-modules --with-json --prefix="${HOME}/opt/emacs-${VERSION}" --bindir="${HOME}/opt/emacs-${VERSION}/bin"

# Build and install
make -j"$(nproc)"
make install

# install alternatives for emacs and emacsclient
sudo update-alternatives --install /usr/bin/emacs emacs "${SRC_PATH}/bin/emacs" 1
sudo update-alternatives --install /usr/bin/emacsclient emacsclient "${SRC_PATH}/bin/emacsclient" 1

# update alternatives to new version
sudo update-alternatives --config emacs
sudo update-alternatives --config emacsclient

cd - || exit
