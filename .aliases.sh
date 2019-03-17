#!/bin/sh
# shellcheck disable=SC2039
# shellcheck disable=SC2155
# shellcheck disable=SC2050

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

sum-lines () {
    awk '{s+=$1} END {print s}'
}

drun () {
    docker run -it --rm "$@"
}

venv () {
    [[ ! -d .venv ]] && python -m venv .venv
    cd . || exit 1
}

# save definition of dot (graphviz language)
alias dot-graph='/usr/bin/dot'
alias dot-tig='GIT_DIR=$HOME/.dot/ tig'


# ustar is my server
if [ "$(hostname)" != "ustar" ]; then
    alias emacs='emacsclient -nw -a lem'
    alias semacs='SUDO_EDITOR="emacsclient -t -a vim" sudoedit'
    alias svim='SUDO_EDITOR=vim sudoedit'
else
    alias semacs='SUDO_EDITOR="emacs" sudoedit'
    alias svim='SUDO_EDITOR="vim" sudoedit'
fi


alias remacs='sudo /etc/init.d/emacs.lerax restart'
alias agenda='gcalcli agenda'
alias dotnet-build='msbuild'
alias sbcl='rlwrap sbcl --noinform'
alias lisp='sbcl'
alias lain=lein
alias dic=sdcv


# don't use nosetests anymore, is legacy broken and fucked
alias nosetests='nose2'
alias deadstar-bind-up='ssh -Y lerax@deadstar -t "x2x -north -to :0.0"'
alias deadstar-bind-left='ssh -Y lerax@deadstar -t "x2x -west -to :0.0"'
alias deadstar-bind-right='ssh -Y lerax@deadstar -t "x2x -east -to :0.0"'
alias deadstar-bind-down='ssh -Y lerax@deadstar -t "x2x -south -to :0.0"'
alias deadstar='ssh lerax@deadstar'
alias starfox-bind-up='ssh -Y lerax@starfox -t "x2x -north -to :0.0"'
alias starfox-bind-left='ssh -Y lerax@starfox -t "x2x -west -to :0.0"'
alias starfox-bind-right='ssh -Y lerax@starfox -t "x2x -east -to :0.0"'
alias starfox-bind-down='ssh -Y lerax@starfox -t "x2x -south -to :0.0"'
alias starfox='ssh lerax@starfox'
alias celeste-bind-up='ssh -Y lerax@celeste -t "x2x -north -completeregionlow 768 -to :0.0"'
alias celeste-bind-left='ssh -Y lerax@celeste -t "x2x -west -completeregionlow 768 -to :0.0"'
alias celeste-bind-right='ssh -Y lerax@celeste -t "x2x -east -completeregionlow 768 -to :0.0"'
alias celeste-bind-down='ssh -Y lerax@celeste -t "x2x -south -completeregionlow 768 -to :0.0"'
alias celeste='ssh lerax@celeste'


# xclipboard
alias xcopy='xclip -selection clipboard'
alias xpaste='xclip -o -selection clipboard'

alias ufc='cd ~/Dropbox/University/Courses/UFC/'

# you need tmate to use that
alias ssh-share='tmux detach -E "tmate && zsh"'

# mouse driver
alias mouse-driver="ratslap"

# making docker life more easy
alias dc=docker
alias sprunge="curl -F 'sprunge=<-' http://sprunge.us"
alias sprungex='sprunge | xcopy'
alias pastebin=ixx

alias tmate.="/usr/bin/tmate"
alias tmate="tmux detach-client -E 'tmate;tmux'"
alias stack-size='ulimit -s'

# pacman aliases
alias pacman-install='sudo pacman -S'
alias pacmani='pacman-install --noconfirm'
alias pacman-update='sudo pacman -Sy'
alias pacman-upgrade='sudo pacman -Syuu'
alias pacman-search='pacman -Ss'
alias pacman-remove='sudo pacman -Rsu'
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
alias thinkpad-keyboard='sudo setxkbmap -model thinkpad60 -layout br'
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
