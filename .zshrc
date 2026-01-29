# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
xhISTSIZE=1000
SAVEHIST=1000
setopt notify
unsetopt beep
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

ls() {
    
    if [ $# -eq 0 ]; then
	command eza --icons --colour=auto --group-directories-first -1
    else
	command eza "$@"
    fi
}

export CYPHAL_PATH="$HOME/.cyphal:$CYPHAL_PATH"

# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
[[ ! -r '/home/jthro/.opam/opam-init/init.zsh' ]] || source '/home/jthro/.opam/opam-init/init.zsh' > /dev/null 2> /dev/null
# END opam configuration

# guix
# export GUIX_PROFILE="$HOME/.guix-profile"
# [ -f "$GUIX_PROFILE/etc/profile" ] && . "$GUIX_PROFILE/etc/profile"

# alias dvtm="dvtm -m ^a"

# haskell
export PATH=$PATH:~/.local/bin/

# cargo
export PATH=$PATH:~/.cargo/bin/

# java
export JAVA_HOME=/usr/lib/jvm/default

# editor
export EDITOR=vim

# qmlls 
export PATH=$PATH:/usr/lib/qt6/bin/

# fnm (node)
eval "$(fnm env --use-on-cd --shell zsh)"

# starship
eval "$(starship init zsh)"
