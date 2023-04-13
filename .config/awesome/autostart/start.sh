#!/bin/bash

[[ ! `pgrep redshift` ]] && redshift -l -3.779500:-49.567810 &
[[ ! `pgrep nm-applet` ]] && nm-applet&
[[ ! `pgrep pulseaudio` ]] && pulseaudio -D &
[[ ! `pgrep blueman-applet` ]] && blueman-applet&
[[ ! `pgrep xfce4-power-manager` ]] && xfce4-power-manager&
[[ ! `pgrep epp-client` ]] && /opt/cososys/bin/epp-client&
[[ ! `pgrep volumeicon` ]] && volumeicon&
[[ `pgrep xfdesktop` ]] && ~/.config/awesome/autostart/xfdesktop-fix.sh
