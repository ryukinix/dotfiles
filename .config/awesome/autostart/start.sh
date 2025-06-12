#!/bin/bash

workstar=PC-002653

[[ ! `pgrep agent` && -f /usr/lib/geoclue-2.0/demos/agent ]] && /usr/lib/geoclue-2.0/demos/agent&
[[ ! `pgrep redshift` ]] && redshift-gtk -l -3.779500:-49.567810 &
[[ ! `pgrep nm-applet` ]] && nm-applet&
[[ ! `pgrep pipewire` ]] && pipewire &
[[ ! `pgrep pipewire-pulse` ]] && pipewire-pulse &
[[ ! `pgrep wireplumber` ]] && wireplumber &
[[ ! `pgrep blueman-applet` ]] && blueman-applet&
[[ ! `pgrep xfce4-power-manager` ]] && xfce4-power-manager&
[[ `hostname` == $workstar &&  ! `pgrep epp-client` ]] && /opt/cososys/bin/epp-client&
[[ ! `pgrep volumeicon` ]] && volumeicon&
[[ `pgrep xfdesktop` ]] && ~/.config/awesome/autostart/xfdesktop-fix.sh
[[ ! $(pgrep polkit-gnome-au) ]] && /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

if [[ `hostname` != $workstar ]]; then
    [[ `rc-service emacs.lerax status` != *started* ]] && sudo /etc/init.d/emacs.lerax restart&
    [[ ! `pgrep pamac-tray` ]] && pamac-tray&
    [[ ! `pgrep udiskie` ]] && udiskie&
fi

# to autolock session after sometime idle
[[ ! $(pgrep xfce4-screensaver) ]] && xfce4-screensaver&
# xfce4-screensaver works better with xfce4-based env (like mine)
# xautolock and light-locker didn't work well
