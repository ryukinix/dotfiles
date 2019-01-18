_economy_services=(
    "docker"
    "ntpd"
    "sshd"
    "NetworkManager"
)

_economy_programs=(
    "geary --hidden"
    "nm-applet"
    "dropbox"
    "telegram-desktop"
    "slack"
)

function _programid {
    echo $1 | cut -d ' ' -f1
}

function economy-run-services {
    for serv in $_economy_services; do
        echo "[info] enabling '$serv' server"
        sudo rc-service $serv start
    done
}

function economy-kill-services {
    for serv in $_economy_services; do
        echo "[info] disabling '$serv' server"
        sudo rc-service $serv stop
    done
}

function economy-run-programs {
    for program in $_economy_programs; do
        local p=$(_programid "${program}")
        if ! pgrep -f $p > /dev/null; then
            echo "[info] starting '$program' program"
            daemonize $p > /dev/null
        fi
    done
}

function economy-kill-programs {
    for program in $_economy_programs; do
        local p=$(_programid "${program}")
        if pgrep -f $p > /dev/null; then
            echo "[info] killing '$program' program"
            pgrep -f $p | xargs kill
        fi
    done
}

function economy-mode-enable {
    economy-kill-services && economy-kill-programs
    echo "[info] setting to battery mode with tlp"
    sudo tlp bat
}

function economy-mode-disable {
    economy-run-services && economy-run-programs
    echo "[info] setting to AC mode with tlp"
    sudo tlp ac
}
