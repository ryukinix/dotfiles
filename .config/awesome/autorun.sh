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
    /etc/xdg/autostart/pamac-tray.desktop
    /usr/share/applications/xfce4-clipman.desktop
    ~/.config/autostart/Compton.desktop
    ~/.config/autostart/Zeal.desktop
    ~/.config/autostart/volumeicon.desktop
    ~/.config/autostart/geary-autostart.desktop
    ~/.config/autostart/dropbox.desktop
    ~/.config/autostart/fluxgui.desktop
)

function notify-dont-exists {
    notify-send "[autoruns.sh] Not found file" `printf "%s" $1`
}

function filter-exists {
    while read line; do
        [[ -f $line ]] && echo $line || notify-send $line
    done
}

function parse-desktop {
    while read line; do
        grep '^Exec' $line | tail -1 | sed 's/^Exec=//' | sed 's/%.//' | sed 's/^"//g' | sed 's/" *$//g'
    done
}

function filter-not-running {
    while read line; do
        comm=`echo $line | cut -f 1 -d " "`
        [[ ! `pgrep $comm` ]] && echo $line
    done

}

function autorun {
    tasks=`echo ${autostart[@]} | wc -w`
    echo ${autostart[@]} \
        | tr " " "\n" \
        | filter-exists \
        | parse-desktop \
        | filter-not-running \
        | xargs -L 1 -P $tasks bash -c "({in} &> /dev/null &) ; exit 0"

    (xfdesktop > /dev/null &)&
}


autorun&
