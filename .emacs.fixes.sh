# when run inside emacs make this special
if [ -n "$INSIDE_EMACS" ] || [ -n "$EXWM" ]; then
    if [ -n "$EXWM" ]; then
        export GIT_EDITOR="emacsclient -s exwm"
    else
        export GIT_EDITOR=emacsclient
    fi

    alias emacs=gemacs
    alias thunar='thunar --daemon'
    if [[ "$(basename $SHELL)" == "zsh" ]]; then
        prompt minimal
    fi
fi
