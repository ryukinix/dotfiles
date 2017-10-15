GREEN="$(tput setaf 2)"
RED="$(tput setaf 1)"
RESET="$(tput sgr0)"

function dot {
    git --git-dir=$HOME/.dot/ --work-tree=$HOME $@
}

# args: <color> <message>
function printf-color {
    printf "$1$2 ${RESET}"
}

# args: <message-type> <message>
function echo-info {
    printf-color $GREEN "[$1]"; echo ${@:2}
}
# args: <message-type> <message>
function echo-error {
    printf-color $RED "[$1]"; echo ${@:2}
}
