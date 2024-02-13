#!/bin/bash

[[ ! $(pgrep polkit-gnome-au) ]] && /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
sudo "/etc/init.d/emacs.$(whoami)" restart
