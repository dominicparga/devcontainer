if ( is_machine 'linux' ); then
    # since installing on Ubuntu 18.04 could fail

    # NOTE
    # PATH in shellrc.sh is related to ${_npm_version} and ${_npm_distro}
    # and should be updated in /shell/shellrc.sh or in /custom/shell/shellrc.sh
    _npm_version='v11.10.0'
    _npm_distro='linux-x64'

    _dest_dir="/usr/local/lib/nodejs"


    sudo mkdir -p "${_dest_dir}"
    wget -c https://nodejs.org/download/release/latest/node-${_npm_version}-${_npm_distro}.tar.xz -O - | sudo tar -xJv -C "${_dest_dir}"
fi
