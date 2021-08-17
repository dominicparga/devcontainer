# Download

- Newest version: `git clone git://git.savannah.gnu.org/emacs.git`
- Released version: [GNU FTP](https://ftp.gnu.org/gnu/emacs/)

# Build and install

## Dependencies

- `sudo apt install libxpm-dev libgif-dev libjpeg-dev libpng-dev libtiff-dev libx11-dev libncurses5-dev automake autoconf texinfo libgtk2.0-dev libgtk-3-dev gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev`

## Build

- `export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10`
- `./autogen.sh`
- `./configure --with-modules --with-gnutls --with-rsvg --with-x --with-json --prefix /home/franzef/opt/emacs-27.2 --bindir /home/franzef/opt/emacs-27.2/bin CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer" --with-mailutils --with-native-compilation`
- `make -j${nproc} && make install`

# Install with update-alternatives

Make sure to update the path correctly.

- `sudo update-alternatives --install /usr/bin/emacsclient emacsclient $HOME/opt/emacs-27.2/bin/emacsclient 4`
- `sudo update-alternatives --install /usr/bin/emacs emacs $HOME/opt/emacs-27.2/bin/emacs 3`
