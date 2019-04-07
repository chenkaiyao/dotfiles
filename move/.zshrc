ZSH_THEME="agnoster"
DEFAULT_USER="chenkaiyao"

export ZSH=$HOME/.oh-my-zsh
export LD_LIBRARY_PATH=/usr/local/lib
export TERM="xterm-256color"

DISABLE_AUTO_UPDATE="true"
plugins=(git colored-man-pages zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# z instead of autojump
# [[ -s ~/.autojump/etc/profile.d/autojump.sh ]] && . ~/.autojump/etc/profile.d/autojump.sh
 . `brew --prefix`/etc/profile.d/z.sh

[[ -f ~/.fzf.zsh ]] && source ~/.fzf.zsh
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

# for source liquidprompt && z awesome tools
[[ `uname -s` == "Linux" ]] && . ~/z/z.sh
[[ `uname -s` == "Darwin" ]] && . `brew --prefix`/etc/profile.d/z.sh

# copy from .bash_profile
# Load the shell dotfiles, and then some:
# * ~/.path can be used to extend `$PATH`.
# * ~/.extra can be used for other settings you don’t want to commit.
# for file in ~/.{path,bash_prompt,exports,aliases,functions,extra}; do
for file in ~/.{path,exports,aliases,functions,extra}; do
	[ -r "$file" ] && [ -f "$file" ] && source "$file";
done;

unset file;

