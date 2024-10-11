# Setup fzf
# ---------
if [[ ! "$PATH" == */home/lerax/.fzf/bin* ]]; then
  PATH="${PATH:+${PATH}:}/home/lerax/.fzf/bin"
fi

source <(fzf --zsh)
