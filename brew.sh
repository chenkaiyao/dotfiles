#!/usr/bin/env bash

CURRENT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Set the colours you can use
black='\033[0;30m'
white='\033[0;37m'
red='\033[0;31m'
green='\033[0;32m'
yellow='\033[0;33m'
blue='\033[0;34m'
magenta='\033[0;35m'
cyan='\033[0;36m'

#  Reset text attributes to normal + without clearing screen.
alias Reset="tput sgr0"

# Color-echo.
# arg $1 = message
# arg $2 = Color
cecho() {
  echo -e "${2}${1}"
  # Reset # Reset to normal.
  return
}

# install Command Line Tools
cecho "installing Command Line Tools" $yellow
xcode-select --install

# install pip
cecho "installing pip" $green
if hash pip 1>/dev/null; then
  cecho "pip already installed" $green
else
  curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
  sudo chmod a+w get-pip.py
  sudo -H python get-pip.py
fi
rm get-pip.py

# install Homebrew
cecho "Installing Homebrew" $yellow
if hash brew 1>/dev/null; then
  cecho "Homebrew already installed" $green
else
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  brew doctor
fi

# install Homebrew Cask
cecho "Installing Homebrew Cask" $yellow
if command brew cask 1>/dev/null; then
  cecho "Homebrew Cask already installed" $green
else
  brew tap caskroom/cask
  brew install caskroom/cask/brew-cask
fi

# install Homebrew java
cecho "Installing java" $yellow
brew cask install java

# 使用 brew services start|stop|restart SERVICE_NAME 这样的命令来操作一切终端服务了 <=> LaunchRocket
cecho "Installing Homebrew Services" $yellow
brew tap homebrew/services

# 使用 brew cu 升级 brew cask 安装的 gui 软件 or brew reinstall someGuiApp
cecho "Installing Homebrew Cask" $yellow
brew tap buo/cask-upgrade

# Keep-alive: update existing `sudo` time stamp until the script has finished.
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

cecho "Updating Homebrew" $yellow
# Make sure we’re using the latest Homebrew.
#brew update

# Upgrade any already-installed formulae.
#brew upgrade

# Install GNU core utilities (those that come with OS X are outdated).
# Don’t forget to add `$(brew --prefix coreutils)/libexec/gnubin` to `$PATH`.
brew install coreutils
ln -s /usr/local/bin/gsha256sum /usr/local/bin/sha256sum

# Install some other useful utilities like `sponge`.
brew install moreutils

# Install GNU `find`, `locate`, `updatedb`, and `xargs`, `g`-prefixed.
brew install findutils

# Install GNU `sed`, overwriting the built-in `sed`.
brew install gnu-sed

# Install Bash 4.
# Note: don’t forget to add `/usr/local/bin/bash` to `/etc/shells` before
# running `chsh`.
brew install bash
brew install bash-completion2

# Switch to using brew-installed bash as default shell
if ! fgrep -q '/usr/local/bin/bash' /etc/shells; then
  echo '/usr/local/bin/bash' | sudo tee -a /etc/shells;
  chsh -s /usr/local/bin/bash
fi;

# Install `wget` with IRI support.
brew install wget
brew install grep
brew install openssh
brew install vim

# gtags for emacs
sudo -H pip install pygments
brew install global

# Install other useful binaries.
brew install graphicsmagick
brew install p7zip
brew install tree
brew install webkit2png
brew install https://raw.githubusercontent.com/kadwanev/bigboybrew/master/Library/Formula/sshpass.rb

# custom my command line tools
brew install curl --force
brew install git --force
brew install ctags
brew install fzf
brew install trash
brew install pcre
brew install openssl
brew install liquidprompt
brew install z
brew install zsh
brew install zsh-completions
brew install graphviz
brew install htop-osx
brew install plantuml
brew install the_silver_searcher

# TODO: 骇客帝国里的屏幕: https://codeburst.io/install-and-setup-cmatrix-on-mac-a2076daee420
brew install cmatrix

# Terminal proxy to Sock5 Shadowsocks
brew install polipo

# emacs support library for pdf
#brew install pdf-tools

# cool movies download from youtube
brew install youtube-dl

# library for shadowsocks
brew install libsodium

# Remove outdated versions from the cellar.
cecho "========>> brew cleanup starting !!! <<========" $yellow
brew cleanup
cecho "========>> brew install finished !!! <<========" $yellow

# refer: https://github.com/junegunn/fzf
cecho "fzf deloying ..." $yellow
/usr/local/opt/fzf/install
complete -F _fzf_file_completion -o default -o bashdefault doge

apps=(
  apptivate # a cool app switcher
  alfred
  caffeine
#  appcleaner
  cheatsheet
  emacs
  gas-mask
  google-chrome
  iterm2
  java
  qq
  smcfancontrol
#  thunder
  vlc
)
cecho "Install My Favorate Apps with brew cask install xxx" $yellow
for item in ${apps[@]}; do
  cecho "> ${item}" $magenta
done
cecho "Enter: y or n To install or not install apps" $yellow
select yn in "Yes" "No"; do
  case $yn in
    Yes )
      cecho "Ok! installing apps, please wait ... " $yellow
      brew cask install --appdir="/Applications" ${apps[@]}
      break;;
    No ) break;;
  esac
done

read -p "Shadowsocks client of python version PK G-F-W ? (y/n) " -n 1;
if [[ $REPLY =~ ^[Yy]$ ]]; then
  #sudo -H pip install shadowsocks
  pip install --upgrade git+https://github.com/shadowsocks/shadowsocks.git@master
  echo -e "\033[40;32m deploy the proxy server on your remote vps: server[1,2,3] \033[0m"
  SS_CFG="/etc/shadowsocks.json"
  if [ ! -f "$SS_CFG" ]; then
    echo "no found shadowsocks config file, touching file: /etc/shadowsocks.json";
    sudo touch "$SS_CFG"
  fi
  sudo chmod a+w "$SS_CFG"

  cat > "$SS_CFG" <<EOF
  {
    "server":["server1","server2"],
    "server_port":8080,
    "local_address":"127.0.0.1",
    "local_port":1080,
    "password":"password",
    "timeout":300,
    "method":"chacha20-ietf-poly1305",
    "fast_open": false
  }
  EOF
  brew services restart polipo
  echo -e "\033[40;32m you can start the shadowsocks server on remote vps: sudo ssserver -c /etc/shadowsocks.json -d start \033[0m"
  echo -e "\033[40;32m you can start the shadowsocks client on your local laptop: sslocal -c /etc/shadowsocks.json \033[0m"
  echo "now, brew services start polipo use the cfg  ~/.polipo, 建议使用默认的配置文件，/usr/local/opt/polipo/homebrew.mxcl.polipo.plist"
fi;

read -p "Oh-My-ZSH ? [y/n]" -n 1;
if [[ $REPLY =~ ^[Yy]$ ]]; then
  sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
fi;

# sudo touch /etc/sysctl.conf
# sudo chmod a+w "/etc/sysctl.conf"
# cat > "/etc/sysctl.conf" <<EOF
# kern.maxfiles=1048600
# kern.maxfilesperproc=1048576
# net.inet.ip.portrange.first=1024
# net.inet.ip.portrange.last=65535
# EOF
cecho "Install Color Scheme for Gnome Terminal and Pantheon Terminal" $green
bash -c  "$(curl -sLo- https://git.io/vQgMr)"

cecho "Done!!! you can deploy vim( ./vim.sh ) or emacs( ./emacs.sh ) to bring you into cool coding environment!!!" $green

