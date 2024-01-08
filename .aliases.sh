#!/bin/bash
# shellcheck disable=SC2039
# shellcheck disable=SC2155
# shellcheck disable=SC2050

dosrun () {
    dosbox "$@" -fullscreen -exit
}

loadenv () {
    local env_file="$1"
    export $(cat ${env_file} | xargs)
}

emacs-lite () {
    local lite="~/.emacs.d.lite"
    emacs -q --eval "(setq user-emacs-directory \"$lite\")" \
          -l "$lite/init.el" "$@"
}

emacs-prelude () {
    local prelude="$HOME/.emacs.d.prelude"
    emacs -q --eval "(setq user-emacs-directory \"$prelude\")" \
          -l "$prelude/init.el" "$@"
}

set-default-emacs () {
    local emacs_type="$1"
    local emacs_d="$HOME/.emacs.d"
    local new_emacs_d="$HOME/.emacs.d.${emacs_type}"
    if [[ ! -d "$new_emacs_d" ]]; then
        printf "error: dir $new_emacs_d not found"
        return 1
    fi

    if [[ -L "${emacs_d}" ]]; then
        unlink $emacs_d
    elif [[ -d "${emacs_d}" ]]; then
        echo "error: ${emacs_d} not a symlink, not a modular emacs config"
        return 1
    fi

    ln -r -v -s "${new_emacs_d}" "${emacs_d}"
}

gclone () {
    local repo="$1"
    git clone "git@github.com:$repo.git" "${@:2}"
}

function chroot-bind {
    sudo mount --rbind /dev dev
    sudo mount --rbind /run run
    sudo mount --rbind /proc proc
    sudo mount --rbind /sys sys
    sudo mount --rbind /home home
}

function dropbox-bind {
    local host="$1"
    mkdir -p ~/Dropbox
    sshfs ${host}:Dropbox ~/Dropbox
}

function ftp-start {
    sudo service pure-ftpd start
}

function ftp-stop {
    sudo service pure-ftpd stop
}

function ftp-status {
    sudo service pure-ftpd status
}

function hack-chat {
    docker run -d --name hchat \
           -p 80:8080 \
           -p 6060:6060 \
           -e WSPROTOCOL="ws://" \
           -e WSPORT="6060" \
           -e WSBASEURL="" \
           -e ADMIN_NAME="boop" \
           -e PASSWORD="pass" \
           -e SALT="2dSg4kS" \
           mcgriddle/hack-chat:latest
}

restart-network () {
    killall -q -9 nm-applet || true
    sudo rc-service -s NetworkManager stop
    sudo rc-service net.wlp3s0 restart
}

shutdown-docker () {
    sudo rc-service docker stop
    sudo ip link del dev docker0 2>/dev/null || true
}

cl-jupyter () {
    docker run \
           --network=host \
           -it \
           neowaylabs/common-lisp-jupyter \
           jupyter notebook --ip=127.0.0.1
}

u-root-boot () {
    sudo qemu-system-x86_64 \
         -kernel /boot/vmlinuz-linux \
         -initrd /tmp/initramfs.linux_amd64.cpio \
         -append "console=ttyS0" \
         -nographic \
         -no-reboot \
         -net nic,model=rtl8139

}

daemonize () {
    ("$@" &> /dev/null&) &> /dev/null&
}


filter-prime () {
    factor "$@" | grep -E '^(.*): \1$' | cut -d':' -f 2 | tr -d ' '
}


# function to run .el files
emacs-run () {
    emacsclient -e "(load '$(pwd)/$1')"
}

common-lisp-jupter () {
    docker run --network=host -it \
           neowaylabs/common-lisp-jupyter jupyter \
           notebook --ip=127.0.0.1
}

# receive the first argument, eval it and copy the result to clipboard.
# used to generate easy functions pastebin-like, as termbin and ixx
_pastebin-generic () {
    link=$(eval "$1" 2> /dev/null | tr -d "\n")
    # shellcheck disable=SC2181
    if [ "$?" -eq '0' ]; then
        printf "%s" "$link" | xclip -selection clipboard
        printf "Copied '%s' to X clipboard.\n" "$link"
    fi
}

termbin () {
    _pastebin-generic 'nc termbin.com 9999'
}

ixx () {
    _pastebin-generic 'ix'
}


dot () {
    GIT_DIR=$HOME/.dot GIT_WORK_TREE=$HOME git "$@"
}

sys () {
    GIT_DIR=/cfg GIT_WORK_TREE=/ git "$@"
}


dotk () {
    GIT_DIR=$HOME/.dot GIT_WORK_TREE=$HOME gitk "$@"
}

dotg () {
    GIT_WORK_TREE=$HOME gitg $HOME/.dot "$@"
}

env-up () {
    # shellcheck disable=SC1090
    source "$1/bin/activate"
}

s3-mkdir () {
    local bucket="$1"
    local folder="$2"
    aws s3api put-object --bucket "$bucket" --key "$folder"
}


gccrun () {
    gcc "$@" -o a.out; ./a.out; rm -f a.out
}

gateway () {
    route | head -n 3 | tail -n 1 | awk '{print $2}'
}

check-ssh-agent-pid () {
    local LIST=$(echo "$(pgrep ssh-agent)" "$(pgrep gnome-keyring-d)" | tr '\n' ' ')
    local VALUE="$SSH_AGENT_PID"
    local SEARCH=$(echo "$LIST" | xargs -n1 echo | grep -E "^$VALUE\$")
    # echo LIST: $LIST
    # echo VALUE: $VALUE
    # echo SEARCH: $SEARCH
    echo "$SEARCH"
}

ssh-auth () {
    local ssh_agent_pid=$(check-ssh-agent-pid)
    local ssh_session_file=/tmp/ssh

    if [[ $SSH_AUTH_SOCK == *"/keyring/ssh" ]]; then
        echo "[info] SSH auth with gnome-keyring-daemon."
    else
        if [[ -z $(check-ssh-agent-pid) ]]; then
            # read session-wise variable
            if [ -f /tmp/ssh ]; then
                # shellcheck disable=SC1091
                source /tmp/ssh
                echo "[info] read /tmp/ssh variable sessions"
            fi
            # try again check ssh agent-pid
            ssh_agent_pid=$(check-ssh-agent-pid)
        fi

        if [[ -z "$ssh_agent_pid" || ! ssh-add > /dev/null ]]; then
            # delete old variables
            rm -rf /tmp/ssh
            # kill old agents
            echo "[info] kill all currently ssh-agent"
            killall ssh-agent

            # create new session
            echo "[info] spawn new ssh-agent"
            eval "$(ssh-agent)"
            export SSH_AGENT_PID
            export SSH_AUTH_SOCK

            # save variables session-wise to filesystem
            touch $ssh_session_file
            echo "export SSH_AGENT_PID=$SSH_AGENT_PID" >> $ssh_session_file
            echo "export SSH_AUTH_SOCK=$SSH_AUTH_SOCK" >> $ssh_session_file
            echo "[info] saved ssh auth variables into $ssh_session_file "
            # add ssh key
            echo "[info] add default ssh key"
            ssh-add
        elif [ -z "$(ssh-add -l)" ]; then
            # if there is a agent, but no keys, just add it
            echo "[info] found a ssh agent working, but there is no keys."
            ssh-add
        else
            echo "[info] SSH auth ok!"
        fi
    fi
}

ssh-remember () {
    # use seahorse to remember password
    /usr/lib/x86_64-linux-gnu/seahorse/seahorse-ssh-askpass ~/.ssh/id_rsa > /dev/null
}

sum-lines () {
    awk '{s+=$1} END {print s}'
}

drun () {
    docker run -it --rm "$@"
}

venv () {
    [[ ! -d .venv ]] && python3 -m venv .venv
    cd . || exit 1
}

git-remember() {
    # cache my https password for 1 week
    git config --global credential.helper 'cache --timeout=604800'
}

git-branch-clean() {
    git checkout master
    git remote prune origin
    if [[ $1 == '-f' ]]; then
        git branch | egrep -v "(^\*|master|dev)" | xargs -r git branch -D
    else
        git branch --merged | egrep -v "(^\*|master|dev)" | xargs -r git branch -d
    fi
}

research () {
    local f="$1"
    ln -v -s "$(readlink -f "$f")" /home/lerax/Dropbox/University/TCC/research/
}

# save definition of dot (graphviz language)
alias dot-graph='/usr/bin/dot'
alias dot-tig='GIT_DIR=$HOME/.dot/ tig'


# ustar is my server
if [ "$(hostname)" != "ustar" ]; then
    alias emacs='emacsclient -nw -a emacs'
    alias semacs='SUDO_EDITOR="emacsclient -t -a vim" sudoedit'
    alias svim='SUDO_EDITOR=vim sudoedit'
else
    alias semacs='SUDO_EDITOR="emacs" sudoedit'
    alias svim='SUDO_EDITOR="vim" sudoedit'
fi


alias remacs='sudo /etc/init.d/emacs.lerax restart'

if which systemctl &> /dev/null; then
   alias remacs='systemctl --user restart emacs'
fi

alias agenda='gcalcli agenda'
alias dotnet-build='msbuild'
alias sbcl='rlwrap sbcl --noinform'
alias lisp='sbcl'
alias lain=lein
alias dic=sdcv


# don't use nosetests anymore, is legacy broken and fucked
alias nosetests='nose2'
alias workstar-bind-up='ssh -Y lerax@workstar -t "x2x -north -to :0.0"'
alias workstar-bind-left='ssh -Y lerax@workstar -t "x2x -west -to :0.0"'
alias workstar-bind-right='ssh -Y lerax@workstar -t "x2x -east -to :0.0"'
alias workstar-bind-down='ssh -Y lerax@workstar -t "x2x -south -to :0.0"'
alias workstar='ssh lerax@workstar'
alias starfox-bind-up='ssh -C -Y lerax@starfox -t "x2x -north -to :0.0"'
alias starfox-bind-left='ssh -C -Y lerax@starfox -t "x2x -west -to :0.0"'
alias starfox-bind-right='ssh -C -Y lerax@starfox -t "x2x -east -to :0.0"'
alias starfox-bind-down='ssh -C -Y lerax@starfox -t "x2x -south -to :0.0"'
alias starfox='ssh lerax@starfox'
alias celeste-bind-up='ssh -C -Y lerax@celeste -t "x2x -north -completeregionlow 768 -to :0.0"'
alias celeste-bind-left='ssh -C -Y lerax@celeste -t "x2x -west -completeregionlow 768 -to :0.0"'
alias celeste-bind-right='ssh -C -Y lerax@celeste -t "x2x -east -completeregionlow 768 -to :0.0"'
alias celeste-bind-down='ssh -C -Y lerax@celeste -t "x2x -south -completeregionlow 768 -to :0.0"'
alias celeste='ssh lerax@celeste'


# xclipboard
alias xcopy='xclip -selection clipboard'
alias xpaste='xclip -o -selection clipboard'

function xcopy-ssh {
    local host="$1"
    ssh -q -X "$host" -t "DISPLAY=:0 xclip -o -selection clipboard" \
        | xcopy
}

alias ufc='cd ~/Dropbox/University/Courses/UFC/'

# you need tmate to use that
alias ssh-share='tmux detach -E "tmate && zsh"'

# mouse driver
alias mouse-driver="ratslap"

# making docker life more easy
alias dc=docker
alias dcp='docker-compose'
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"
alias sprungex='sprunge | xcopy'
alias pastebin=ixx

alias tmate="tmux detach-client -E 'tmate;tmux'"
alias stack-size='ulimit -s'

# pacman aliases
alias pacman-install='sudo pacman -S'
alias pacman-update='sudo pacman -Sy'
alias pacman-upgrade='sudo pacman -Syuu'
alias pacman-search='pacman -Ss'
alias pacman-remove='sudo pacman -Rsu'
alias pcmi='pacman-install'
alias pcmu='pacman-update'
alias pcmuu='pacman-upgrade'
alias pcms='pacman-search'
alias pcmr='pacman-remove'
alias service='sudo rc-service'
alias aur=yay
alias aur-install='aur -S'
alias aur-upgrade='aur -Syuua --overwrite "*"'

alias examples=tldr
alias monitor-off='xset -display :0.0 dpms force off'
alias monitor-on='xset -display :0.0 dpms force on'

alias pdf=llpp
alias qemu=qemu-system-x86_64
alias lelerax='cd ~/Desktop/workspace/lelerax; ./repl'
alias encrypt='gpg -se -r manoel'
alias decrypt='gpg -q --decrypt'

# pip install ipykernel
alias jupyter-create-kernel='ipython kernel install --user --name'
alias distro='cat /etc/os-release | grep "^NAME=" | cut -d"=" -f 2 | tr -d \"'
alias telegram-disable='chmod -x ~/.Telegram/Telegram'
alias telegram-enable='chmod +x ~/.Telegram/Telegram'


alias env-down='deactivate'
alias pyenv-init='eval "$(pyenv init -)"'
alias pipi='pip install --user'
alias rot13="tr 'A-Za-z' 'N-ZA-Mn-za-m'"
alias xfdesktop-shadow-fix='xfconf-query -c xfce4-desktop -p /desktop-icons/center-text -n -t bool -s false'
alias docker-clean='docker system prune'
alias android-up='jmtpfs'
alias android-down='fusermount -u'
alias json=gron
alias reload-aliases='source ~/.aliases.sh'
alias battery=acpi
alias ungron='gron --ungron'
alias histogram='sort | uniq -c'
alias scp-continue='rsync -P -e ssh'
alias hcf='sudo halt'
alias keyboard-thinkpad='setxkbmap -model thinkpad60 -layout br'
alias keyboard-abnt2='setxkbmap -model abnt2 -layout br'
alias open='xdg-open'
alias ssh-ustar="gcloud compute ssh --zone=us-central1-c lerax@ustar"
alias duhere="du -h -d 1 | sort -h -k 1"
alias git-amend="git commit --amend --no-edit"
alias merge='meld'
alias artix-news-summary='artix-news -s'
alias awesome-restart='echo awesome.restart() | awesome-client'
alias no='yes | tr "y" "n"'
alias ge='gemacs'
alias git-push-all='git remote | xargs -L 1  -I@ git push'
alias python-server='python -m http.server'
alias dr='docker run --rm -it'
alias chown-here='chown -R lerax:lerax .'
alias ustar='ssh ustar'
alias networks='wicd-curses'
alias drive-upgrade='go get -u github.com/odeke-em/drive/cmd/drive'
alias ap='create_ap'
alias benchmark='hyperfine'
alias gcloud-shell='gcloud alpha interactive'
alias startxemacs='tmux detach-client -E startx'
alias terminal='xfce4-terminal'
alias rasp='ssh rasp'
alias pip='pip3'
alias python='python3'
alias commits='git --no-pager log --oneline master..@'
alias tunnels="lsof -i -n | egrep '^ssh\b'"
alias celeste-resolution="xrandr --output LVDS1 --mode 1366x768 --rate 60"
alias git-commit-stage="git reset --soft @~1"
alias makedeb="sudo checkinstall"
alias ptime="ps -o etime= -p"
alias rsync-ssh-push='rsync -r ~/.ssh /keybase/private/lerax/.ssh'
alias rsync-ssh-pull='rsync -r /keybase/private/lerax/.ssh ~/.ssh'
alias wemacs='ssh -Y workstar -t gemacs -c'
alias git-message='git log --format=%B -n 1'
alias dcshell='docker run --rm -it --entrypoint=/bin/bash'
alias battery-headset='bluetooth_battery $(bluetoothctl paired-devices | grep TUNE500BT | cut -d " " -f2).1'
alias pulseaudio-restart='pulseaudio -k; pulseaudio -D'
alias telegram-off='sudo chmod -x `which telegram-desktop | head -n 1`'
alias telegram-on='sudo chmod +x `which telegram-desktop | head -n 1`'
alias awesome-quit="awesome-client 'awesome.quit()'"
alias tcc='cd ~/Dropbox/University/TCC'
alias python-linters-install='pip install flake8 mypy black black-macchiato'
alias docker-kill-all="docker ps | awk '{print \$1}' | tail -n +2 | xargs docker kill"
alias prun='pdm run'
alias raspberry-bind='ssh -C -Y lerax@raspberry -t "x2x -north -to :0.0"'
alias dropbox-ignore='attr -s com.dropbox.ignored -V 1'
alias dropbox-ignore-undo='attr -r com.dropbox.ignored'
alias raspberry="ssh lerax@raspberry"

# ref: https://www.reddit.com/r/gnome/comments/h9drsb/disable_superp_or_change_default_display_layout/
alias disable-super-p="gsettings set org.gnome.mutter.keybindings switch-monitor \"['XF86Display']\""
alias enable-super-p="gsettings reset org.gnome.mutter.keybindings switch-monitor"
alias fix-wayland-not-show-snap-apps="gsettings reset org.gnome.shell app-picker-layout"


alias apps="cd ~/RetroPie/roms/apps"
alias raspi-temp="vcgencmd measure_temp | egrep -o '[0-9]*\.[0-9]*'"
alias retropie-setup="cd ~/Desktop/RetroPie-Setup/; sudo ./retropie_setup.sh"
alias dbx=distrobox
alias workspace="cd ~/Desktop/workspace"
alias ubuntu-upgrade="sudo apt update; sudo apt upgrade -y; sudo apt dist-upgrade -y"
alias apt-remove-all="sudo apt purge --auto-remove"
alias web="cd ~/Dropbox/Programming/Projects/Website/ryukinix.github.io/"
