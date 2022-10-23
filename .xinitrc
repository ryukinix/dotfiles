#!/bin/bash

#
# ~/.xinitrc
#
# Executed by startx
#

export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

xmodmap ~/.Xmodmap

chromium-turbo () {
    exec chromium-browser \
         --high-dpi-support=true \
         --force-device-scale-factor=1.75\
         --start-maximized \
         --window-position=0,0
}

case $1 in
    xfce) exec dbus-launch xfce4-session ;;
    awesome) exec dbus-launch awesome ;;
    emacs) . ~/.exwm.sh ;;
    chromium) chromium-turbo ;;
esac
