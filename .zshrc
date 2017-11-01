#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

function safe-source {
    [[ -f $1 ]] && source $1
}

# Customize to your needs...
safe-source ~/.aliases.sh
safe-source ~/.sudo.plugin.zsh
safe-source ~/.emacs.fixes.sh
safe-source ~/.tmate.sh
[[ `which fzf` ]] && safe-source ~/.fzf.sh
