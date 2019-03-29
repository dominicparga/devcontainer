################################################################################
# update brew first

if ( which brew 1>/dev/null 2>/dev/null ); then
    echo "Installing homebrew..."
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
else
    echo "Updating homebrew and formulae..."
    brew update
    brew upgrade
    brew cask upgrade
    brew cleanup
fi

################################################################################
# define and install formulae

# important order
brew cask install java
brew install scala
brew install sbt

# unordinary formulae in alphabetical order
brew cask install alfred
brew install coreutils
brew cask install discord
brew cask install dropbox
brew cask install enpass
brew cask install firefox
brew cask install gimp
brew install git
brew install grep
brew install htop
brew cask install microsoft-office
brew cask install musescore
brew install neovim
brew install node
brew install python
brew install python@2
brew cask install spotify
brew cask install teamspeak-client
brew cask install Telegram
brew install tree
brew cask install visual-studio-code
brew cask install vlc
brew install zsh
brew install heroku/brew/heroku

# needs extra stuff
brew cask install virtualbox
brew cask install teamviewer # needs password
