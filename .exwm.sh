#!/bin/sh

setup_keyboard () {
    if [ "$(hostname)" = "starfox" ]; then
        setxkbmap -model abnt2 -layout br
        setxkbmap -option compose:rctrl
    elif [ "$(hostname)" = "celeste" ]; then
        setxkbmap -model thinkpad60 -layout br
        setxkbmap -option
    fi
    xmodmap ~/.Xmodmap
}


start_daemons () {
    eval "$(gnome-keyring-daemon --start --components=ssh)"
    gnome-keyring-daemon --start --components=pkcs11 &
    gnome-keyring-daemon --start --components=secrets &
    /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
    export SSH_AUTH_SOCK
}

exwm () {
    export EXWM=1
    # Disable access control for the current user.
    xhost "+SI:localuser:$USER"

    # Make Java applications aware this is a non-reparenting window manager.
    export _JAVA_AWT_WM_NONREPARENTING=1

    # Set default cursor.
    xsetroot -cursor_name left_ptr

    # Set keyboard repeat rate.
    xset r rate 200 60

    # Finally start Emacs
    # exec dbus-launch emacs --eval "(lerax-exwm-start nil t)"
    exec emacsclient --create-frame -a "emacs --fullscreen" --eval "(toggle-frame-fullscreen)"

}

# start_daemons
# setup_keyboard
exwm
