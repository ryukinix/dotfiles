#!/usr/bin/env bash

set -ux
w=1438
h=858
counter=0
limit=300
regex='^Desktop$'


win=$(xdotool search --name $regex)
while [ "$win" = "" ]; do
    win=$(xdotool search --name $regex)
    counter=$((1+counter))
    if [ "$counter" -gt $limit ]; then
        echo "Window not found" > /dev/stdeer
        break
    fi
    sleep 0.1s
done

sleep 1s
xdotool windowmove "$win" 0 20
xdotool windowsize "$win" $w $h
