#!/bin/bash
GREEN="\033[0;32m"
RED="\033[0;31m"
RESET="\033[0m"

function dot {
    git --git-dir=$HOME/.dot/ --work-tree=$HOME $@
}

# only send to stdout inputs which are directory or file
function exists {
    while read f;
    do
        if [[ -f $f || -d $f ]]; then
            echo $f
        fi
    done
}

# args: <color> <message>
function printf_color {
    printf "$1$2 ${RESET}"
}

# args: <message-type> <message>
function echo_info {
    printf_color $GREEN "[$1]"; echo ${@:2}
}
# args: <message-type> <message>
function echo_error {
    printf_color $RED "[$1]"; echo ${@:2}
}

function command_exists {
    command -v $1 > /dev/null
}
