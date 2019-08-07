# Setup fzf
# ---------
if [[ ! "$PATH" == */home/manoelvilela/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/home/manoelvilela/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/lerax/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/lerax/.fzf/shell/key-bindings.bash"
