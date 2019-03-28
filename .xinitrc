#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx
#

export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

xmodmap ~/.Xmodmap


legacy() {
    ## FIXME: this doesn't works!
    case $1 in
        xfce) exec dbus-launch xfce4-session ;;
        awesome) exec dbus-launch awesome ;;
        emacs) exwm ;;
        exwm) exwm ;;
        *) exec dbus-launch $1 ;;
    esac
}



. ~/.exwm.sh
