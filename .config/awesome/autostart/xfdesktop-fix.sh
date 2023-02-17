#!/usr/bin/env bash

set -ux
w=3286
h=1028
counter=0
limit=500
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

for i in `seq 1 6`; do
    xdotool windowmove "$win" 0 20
    xdotool windowsize "$win" $w $h
    sleep 5s
done
