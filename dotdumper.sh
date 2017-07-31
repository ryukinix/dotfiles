#!/bin/bash
# -*- coding: utf-8 -*-
#
#    Copyright © Manoel Vilela 2016
#
#    @project: Dotfiles
#     @author: Manoel Vilela
#      @email: manoel_vilela@engineer.com
#

# WARNING: this script is deprecated and no longer supported, try it by yourself. 
#          If you found some bugs and fuckup your system let me be clear: you're alone.

FOLDERS=( .fonts .emacs.d .conky .config
          .irssi .lein .jupyter )
FILES=( .viminfo .bashrc .vimrc.local .emacs-live.el 
        .git-prompt.sh .vimrc .gdbinit .Xmodmap)

printf '\e[01;31m______ _____ ___________ _   ____  _________ ___________\e[00;00m\n'
printf '\e[01;31m|  _  \  _  |_   _|  _  \ | | |  \/  || ___ \  ___| ___ \ \e[00;00m\n'
printf '\e[01;31m| | | | | | | | | | | | | | | | .  . || |_/ / |__ | |_/ /\e[00;00m\n'
printf '\e[01;31m| | | | | | | | | | | | | | | | |\/| ||  __/|  __||    / \e[00;00m\n'
printf '\e[01;31m| |/ /\ \_/ / | | | |/ /| |_| | |  | || |   | |___| |\ \ \e[00;00m\n'
printf '\e[01;31m|___/  \___/  \_/ |___/  \___/\_|  |_/\_|   \____/\_| \_|\e[00;00m\n'
echo
echo
echo
echo
echo
# Make sure only root can run our script
if [ "$(id -u)" != "0" ]; then
   printf "\e[01;33mwarning: this script must be run as root\n" 1>&2
   printf "trying again as: sudo -E $0\n\e[00;00m"
   sudo -E $0
   exit 1
fi                                                        
                                                         
# get the folder to bind & hardlinking
printf "λ=> put repository path for dump dotfiles: "
read TARGET

if [ "$TARGET" == "" ]; then
    echo "[warning] empty entry, assuming folder `pwd`"
    TARGET=`pwd`
fi

# creating & binding target folders
printf "\e[01;36mλ=> Creating folders & Binding\e[00;00m\n"
for folder in "${FOLDERS[@]}";
do
    mkdir -p $TARGET/$folder && mount --bind ~/$folder $TARGET/$folder 2> \
           >(while read line; do echo -e "\e[01;31m$line\e[0m" >&2; done)
    if [ "$?" == '0' ]; then
        echo "~/$folder -> $TARGET/$folder"
    fi
done

# hardlinks force for files
printf "\e[01;36mλ=> Hardlink files\e[00;00m\n"
for file in "${FILES[@]}";
do
    
    ln -f ~/$file $TARGET/ 2> \
        >(while read line; do echo -e "\e[01;31m$line\e[0m" >&2; done)
    if [ $? == '0' ]; then
        echo "~/$file -> $TARGET/$file"
    else
        echo "failed to copy ~/$file"
    fi
done
