# Setup fzf
# ---------
if [[ ! "$PATH" == */home/lerax/.fzf/bin* ]]; then
  PATH="${PATH:+${PATH}:}/home/lerax/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/lerax/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/lerax/.fzf/shell/key-bindings.bash"
