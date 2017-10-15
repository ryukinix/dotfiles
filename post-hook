#!/bin/sh
#
# just ignore all shit files after `git pull`
#

ignore=(~/install.sh ~/README.md ~/setup.sh  ~/post-merge-hook.sh)

mkdir -p ~/.dot-backup/
mv ${ignore[@]} ~/.dot-backup/ 2> /dev/null || true
/usr/bin/git --git-dir=$HOME/.dot/ --work-tree=$HOME \
             update-index --assume-unchanged ${ignore[@]}
