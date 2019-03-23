hide_desktop_icons() {
    defaults write com.apple.finder CreateDesktop -bool false \
    && killall Finder
}
