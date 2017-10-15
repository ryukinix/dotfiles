#!/bin/bash

cd ~/

if [ -d .dot ]; then
  echo "Already installed dotfiles"
  exit 1
fi

git clone --bare git@github.com:ryukinix/dotfiles.git $HOME/.dot --recursively

function dot {
  git --git-dir=$HOME/.dot/ --work-tree=$HOME $@
}

mkdir -p .dot-backup

dot checkout

if [ $? = 0 ]; then
  echo "Checked out dot."
else
  echo "Backing up pre-existing dot files."

  rm -rf .dot-backup
  for file in $(dot checkout 2>&1 | egrep "^\s+" | awk {'print $1'})
  do
    mkdir -p $(dirname .dot-backup/$file)
    echo "Backup $file => .dot-backup/$file"
    mv $file .dot-backup/$file
  done
fi

dot reset HEAD . > /dev/null
dot checkout . > /dev/null
dot config status.showUntrackedFiles no



echo "Dotfiles installed"

bash install.sh

# ignore loop (remove files which don't belong to dotfiles)
for i in README.md setup.sh install.sh dotdumper.sh; do
    git rm
    git update-index --assume-unchanged $i
done
