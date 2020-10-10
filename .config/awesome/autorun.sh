#!/bin/bash
# Setup Xorg a little bit after awesomewm starts

# MANOEL HELL HERE, be careful or just delete this

if [[ $(hostname) == "starfox" ]]; then
    setxkbmap -model abnt2 -layout br
    setxkbmap -option compose:rctrl
elif [[ $(hostname) == "celeste" ]]; then
    setxkbmap -model thinkpad60 -layout br
    setxkbmap -option
fi

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
    echo $userresources loading
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    echo $usermodmap loading
    xmodmap "$usermodmap"
fi


# Setup some stuff based on xfce4 autostart files
autostart=(
    /etc/xdg/autostart/gnome-keyring-pkcs11.desktop
    /etc/xdg/autostart/gnome-keyring-secrets.desktop
    /etc/xdg/autostart/gnome-keyring-ssh.desktop
    /etc/xdg/autostart/xfce-polkit-gnome-authentication-agent-1.desktop
    /etc/xdg/autostart/at-spi-dbus-bus.desktop
    /usr/share/applications/xfce4-clipman.desktop
    ~/.config/autostart/Compton.desktop
    ~/.config/autostart/dropbox.desktop
)

commands=(
    "xfdesktop --disable-wm-check"
    "thunar --daemon"
    "pulseaudio -D"
    "volumeicon"
    "run_keybase -g -a" # keybase kbfs no gui
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
    xargs -L 1 -I{in} -P $1 bash -c "({in} &> /dev/null &)"
}

function run-desktop {
    local tasks=$1
    if [[ -z $tasks ]]; then
        tasks=1
    fi
    filter-exists \
        | parse-desktop \
        | filter-not-running \
        | execution-pool $tasks
}

function run-commands {
    local tasks=$1
    if [[ -z $tasks ]]; then
        tasks=1
    fi
    filter-exists \
        | filter-not-running \
        | execution-pool $tasks

}

function autorun {
    tasks=`echo "${autostart[@]}" | wc -w`
    array-to-lines "${autostart[@]}" | run-desktop $tasks
    array-to-lines "${commands[@]}" | run-commands

    # NOTE: !WARNING!
    # local-autostart.txt should have a input like $autostart array
    # variable one entry per line of a .desktop absolute path.
    local local_autostart=~/.config/awesome/local-autostart.txt
    if [[ -f $local_autostart ]]; then
        cat $local_autostart | grep -i ".desktop$" | run-desktop 1
        cat $local_autostart | grep -i ".sh$" | run-commands 1
    fi
}

autorun
