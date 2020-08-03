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
safe-source ~/.python-venv-autoswitch.zsh
safe-source ~/.economy-mode.sh
safe-source ~/.neoway.sh
safe-source ~/.local.sh
[[ -f `which fzf` ]] && safe-source ~/.fzf.zsh
