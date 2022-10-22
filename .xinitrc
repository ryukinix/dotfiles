#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx
#

export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

xmodmap ~/.Xmodmap

case $1 in
    xfce) exec dbus-launch xfce4-session ;;
    awesome) exec dbus-launch awesome ;;
    emacs) . ~/.exwm.sh ;;
    chromium) exec chromium-browser --start-fullscreen --window-size=1920,1080 --window-position=0,0;;
esac
