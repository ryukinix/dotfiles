#!/bin/sh
#
# just ignore all shit files after `git pull`
#

ignore=(~/install.sh ~/README.md ~/setup.sh ~/dotdumper.sh ~/post-merge-hook.sh)

mkdir -p ~/.dot-backup/
# backup
cp -v ${ignore[@]} ~/.dot-backup/
# remove
rm -rvf ${ignore[@]}
/usr/bin/git --git-dir=$HOME/.dot/ --work-tree=$HOME \
             update-index --assume-unchanged ${ignore[@]}
