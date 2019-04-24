sudo -v

sh ${DOTFILES}/utils/uninstall/homebrew/zsh.sh

/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/uninstall)"

printf "Removing in /usr/local: etc/ var/ Caskroom/ share/ ? (y/n) "
read ANS

if [[ ${ANS} = y ]]
then
    sudo rm -rfv /usr/local/etc
    sudo rm -rfv /usr/local/var
    sudo rm -rfv /usr/local/Caskroom
    sudo rm -rfv /usr/local/share
else
    printf "Do not remove files.\n"
fi
