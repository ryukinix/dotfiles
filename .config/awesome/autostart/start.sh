#!/bin/bash

workstar=PC-002653

[[ ! `pgrep redshift` ]] && redshift -l -3.779500:-49.567810 &
[[ ! `pgrep nm-applet` ]] && nm-applet&
[[ ! `pgrep pipewire` ]] && pipewire &
[[ ! `pgrep pipewire-pulse` ]] && pipewire-pulse &
[[ ! `pgrep wireplumber` ]] && wireplumber &
[[ ! `pgrep blueman-applet` ]] && blueman-applet&
[[ ! `pgrep xfce4-power-manager` ]] && xfce4-power-manager&
[[ `hostname` == $workstar &&  ! `pgrep epp-client` ]] && /opt/cososys/bin/epp-client&
[[ ! `pgrep volumeicon` ]] && volumeicon&
[[ `pgrep xfdesktop` ]] && ~/.config/awesome/autostart/xfdesktop-fix.sh

if [[ `hostname` != $workstar ]]; then
    [[ `rc-service emacs.lerax status` != *started* ]] && sudo /etc/init.d/emacs.lerax restart&
    [[ ! `pgrep pamac-tray` ]] && pamac-tray&
    [[ ! `pgrep udiskie` ]] && udiskie&
fi
