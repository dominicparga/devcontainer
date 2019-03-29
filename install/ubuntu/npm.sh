if ( is_machine 'linux' ); then
    # since installing on Ubuntu 18.04 could fail

    VERSION=v11.11.0
    DISTRO=linux-x64

    _dest_dir="/usr/local/lib/nodejs"


    sudo mkdir -p "${_dest_dir}"
    wget -c https://nodejs.org/download/release/latest/node-${VERSION}-${DISTRO}.tar.xz -O - | sudo tar -xJv -C "${_dest_dir}"
fi
