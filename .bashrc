#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

source ~/.aliases.sh
[[ -f ~/.emacs.fixes.sh ]] && source ~/.emacs.fixes.sh

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
