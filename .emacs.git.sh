# when run inside emacs make this special
if [ -v INSIDE_EMACS ]; then
    export GIT_EDITOR=emacsclient
fi
