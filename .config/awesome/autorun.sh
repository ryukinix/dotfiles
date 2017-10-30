#!/bin/bash
# Setup Xorg a little bit after awesomewm starts

userresources=$HOME/.Xdefaults
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# MANOEL HELL HERE, be careful or just delete this

# Setup some stuff based on xfce4 autostart files
autostart=(
    /etc/xdg/autostart/gnome-keyring-pkcs11.desktop
    /etc/xdg/autostart/gnome-keyring-secrets.desktop
    /etc/xdg/autostart/gnome-keyring-ssh.desktop
    /etc/xdg/autostart/xfce-polkit-gnome-authentication-agent-1.desktop
    /etc/xdg/autostart/at-spi-dbus-bus.desktop
    /usr/share/applications/xfce4-clipman.desktop
    ~/.config/autostart/Compton.desktop
    ~/.config/autostart/Zeal.desktop
    ~/.config/autostart/volumeicon.desktop
    ~/.config/autostart/dropbox.desktop
    ~/.config/autostart/fluxgui.desktop
)

commands=(
    "nm-applet"
    "thunar --daemon"
    "xfdesktop --disable-wm-check"
)

# notify if file dont-exists
function notify-dont-exists {
    notify-send "[autoruns.sh] Not found file" `printf "%s" $1`
}

# only return the arguments which are files that exists on the filesystem
function filter-exists {
    while read line; do
        comm=`echo $line | cut -f 1 -d " "`
        ([[ -f "$line" ]] || command -v "$comm" > /dev/null) && echo $line || notify-send $line
    done
}

# return the Exec line of .desktop files
# should be combined as pipe
function parse-desktop {
    while read line; do
        grep '^Exec' $line | tail -1 | sed 's/^Exec=//' | sed 's/%.//' | sed 's/^"//g' | sed 's/" *$//g'
    done
}

# return only process which are not executing
# first argument need to be the first token a program name
# should be combined as pipe
function filter-not-running {
    while read line; do
        comm=`echo $line | cut -f 1 -d " "`
        [[ ! `pgrep $comm` ]] && echo $line && echo "executing: " $line >&2
    done

}

function array-to-lines {
    printf "%s\n" "$@"
}

# first arg is the size of pool
function execution-pool {
    xargs -L 1 -I{in} -P $1 bash -c "({in} &> /dev/null &) ; exit 0"
}

function autorun {
    tasks=`echo "${autostart[@]}" | wc -w`
    array-to-lines "${autostart[@]}" \
        | filter-exists \
        | parse-desktop \
        | filter-not-running \
        | execution-pool $tasks

    tasks=`echo "${commands[@]}" | wc -w`
    array-to-lines "${commands[@]}" \
        | filter-exists \
        | filter-not-running \
        | execution-pool $tasks

}

autorun