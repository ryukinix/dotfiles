#!/bin/bash

#latlong=""
latlong="-l -3.779500:-49.567810"

[[ ! `pgrep redshift` ]] && redshift ${latlong} &
[[ ! `pgrep nm-applet` ]] && nm-applet&
# [[ ! `pgrep barrier` ]] && barrier&
[[ ! `pgrep pulseaudio` ]] && pulseaudio -D &
[[ ! `pgrep blueman-applet` ]] && blueman-applet&
[[ ! `pgrep xfce4-power-manager` ]] && xfce4-power-manager&
[[ ! `pgrep epp-client` ]] && /opt/cososys/bin/epp-client&
[[ `pgrep xfdesktop` ]] && ~/.config/awesome/autostart/xfdesktop-fix.sh

xrandr --output HDMI-1 --rotate left --auto
