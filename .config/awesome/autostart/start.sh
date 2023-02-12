#!/bin/bash

#latlong="-l -3.779500:-49.567810"
latlong=""

[[ ! `pgrep redshift-gtk` ]] && redshift-gtk ${latlong}&
[[ ! `pgrep nm-applet` ]] && nm-applet&
# [[ ! `pgrep barrier` ]] && barrier&
[[ ! `pgrep pulseaudio` ]] && pulseaudio -D &
[[ ! `pgrep blueman-applet` ]] && blueman-applet&
[[ ! `pgrep xfce4-power-manager` ]] && xfce4-power-manager&
[[ ! `pgrep epp-client` ]] && /opt/cososys/bin/epp-client&
[[ `pgrep xfdesktop` ]] && ~/.config/awesome/autostart/xfdesktop-fix.sh
