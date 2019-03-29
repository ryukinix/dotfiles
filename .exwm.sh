#!/bin/sh

setup_keyboard () {
    if [[ $(hostname) == "starfox" ]]; then
        setxkbmap -model abnt2 -layout br
        setxkbmap -option compose:rctrl
    elif [[ $(hostname) == "celeste" ]]; then
        setxkbmap -model thinkpad60 -layout br
        setxkbmap -option
    fi
}

exwm () {
    export EXWM=1
    # Disable access control for the current user.
    xhost +SI:localuser:$USER

    # Make Java applications aware this is a non-reparenting window manager.
    export _JAVA_AWT_WM_NONREPARENTING=1

    # Set default cursor.
    xsetroot -cursor_name left_ptr

    # Set keyboard repeat rate.
    xset r rate 200 60

    # Finally start Emacs
    exec dbus-launch emacsclient -c --eval "(lerax-exwm-start)" -a ""

}

setup_keyboard
exwm
