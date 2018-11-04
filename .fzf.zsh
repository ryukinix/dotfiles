function safe-load {
    local fpath="$1"
    [[ -f $fpath ]] && source $fpath
}


safe-load /usr/share/fzf/completion.zsh
safe-load /usr/share/fzf/key-bindings.zsh
