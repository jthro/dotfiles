# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt notify
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/jthro/.zshrc'

autoload -Uz compinit
compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
# End of lines added by compinstall

# syntax highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

updatedots() {
  prev_dir="$PWD"
  cd ~/dotfiles/
  stow .
  cd "$prev_dir"
}

# cargo
export PATH=$PATH:~/.cargo/bin/

# fnm (node)
eval "$(fnm env --use-on-cd --shell zsh)"

# starship
eval "$(starship init zsh)"


