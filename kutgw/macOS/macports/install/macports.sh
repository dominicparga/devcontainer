# download

curl -L -o ~/Downloads/MacPorts.pkg https://github.com/macports/macports-base/releases/download/v2.4.2/MacPorts-2.4.2-10.13-HighSierra.pkg

sudo installer -pkg ~/Downloads/MacPorts.pkg -target /

rm ~/Downloads/MacPorts.pkg



# update

sh ${DOTFILES}/utils/update/macports/macports.sh

