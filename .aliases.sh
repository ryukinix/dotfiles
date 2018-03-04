#!/bin/sh

function u-root-boot {
    sudo qemu-system-x86_64 \
         -kernel /boot/vmlinuz-linux-lts \
         -initrd /tmp/initramfs.linux_amd64.cpio \
         -append "console=ttyS0" \
         -nographic \
         -no-reboot \
         -net nic,model=rtl8139

}


function daemonize {
    ($1 &> /dev/null&)&
}


# function to run .el files
function emacs-run {
    emacsclient -e "(load \"$(pwd)/$1\")"
}


function termbin {
    link=`nc termbin.com 9999 2> /dev/null | tr -d "\n"`
    if [ $? -eq '0' ]; then
        printf $link | xclip -selection clipboard
        printf "Copied '%s' to X clipboard.\n" $link
    fi
}

function dot {
    GIT_DIR=$HOME/.dot GIT_WORK_TREE=$HOME git $@
}

# save definition of dot (graphviz language)
alias dot-graph='/usr/bin/dot'
alias dot-tig='GIT_DIR=$HOME/.dot/ tig'


# ustar is my server
if [ $(hostname) != "ustar" ]; then
    alias vim=nvim
    alias emacs='emacsclient -nw -a vim'
    alias semacs='SUDO_EDITOR="emacsclient -t -a vim" sudoedit'
    alias svim='SUDO_EDITOR=vim sudoedit'
else
    alias semacs='SUDO_EDITOR="emacs" sudoedit'
    alias svim='SUDO_EDITOR="vim" sudoedit'
fi


alias remacs='sudo /etc/init.d/emacs.lerax restart'
alias agenda='gcalcli agenda'
alias dotnet-build='msbuild'
alias sbcl='rlwrap sbcl'
alias lisp=sbcl
alias lain=lein
alias dic=sdcv


# don't use nosetests anymore, is legacy broken and fucked
alias nosetests='nose2'
alias deadstar-bind-up='ssh -Y lerax@deadstar -t "x2x -north -to :0.0"'
alias deadstar-bind-left='ssh -Y lerax@deadstar -t "x2x -west -to :0.0"'
alias deadstar-bind-right='ssh -Y lerax@deadstar -t "x2x -east -to :0.0"'
alias deadstar-bind-down='ssh -Y lerax@deadstar -t "x2x -south -to :0.0"'
alias starfox-bind-up='ssh -Y lerax@starfox -t "x2x -north -to :0.0"'
alias starfox-bind-left='ssh -Y lerax@starfox -t "x2x -west -to :0.0"'
alias starfox-bind-right='ssh -Y lerax@starfox -t "x2x -east -to :0.0"'
alias starfox-bind-down='ssh -Y lerax@starfox -t "x2x -south -to :0.0"'
alias starfox='ssh lerax@starfox'
alias deadstar='ssh lerax@deadstar'
alias ustar='ssh server.lerax.me'
alias lerax.me='ssh server.lerax.me'



# xclipboard
alias xcopy='xclip -selection clipboard'
alias xpaste='xclip -o -selection clipboard'

alias ufc='cd ~/Dropbox/University/Courses/UFC/2017.2/'

# you need tmate to use that
alias ssh-share='tmux detach -E "tmate && zsh"'

# mouse driver
alias mouse-driver="ratslap"

# making docker life more easy
alias dc=docker
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"
alias sprungex='sprunge | xcopy'
alias pastebin=termbin

alias tmate.="/usr/bin/tmate"
alias tmate="tmux detach-client -E 'tmate;tmux'"
alias stack-size='ulimit -s'


# pacman aliases
alias pacman-install='sudo pacman -S'
alias pacman-update='sudo pacman -Sy'
alias pacman-upgrade='sudo pacman -Syuu'
alias pacman-search='pacman -Ss'
alias pacman-remove='sudo pacman -Rsu'
alias service='sudo rc-service'
alias pacaur-upgrade='pacaur -Syuua'
alias pacaur-install='pacaur -Sa'


alias aur=trizen
alias examples=tldr
alias monitor-off='xset -display :0.0 dpms force off'
alias monitor-on='xset -display :0.0 dpms force on'

alias pdf=llpp
alias qemu=qemu-system-x86_64
alias lelerax='cd ~/Desktop/workspace/dev/lelerax; ./repl'
alias encrypt='gpg -se'
alias decrypt='gpg -q --decrypt'

# pip install ipykernel
alias jupyter-create-kernel='ipython kernel install --user --name'
alias distro='cat /etc/os-release | grep "^NAME=" | cut -d"=" -f 2 | tr -d \"'
