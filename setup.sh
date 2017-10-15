#!/bin/bash

cd ~/

REPO_NAME=ryukinix/dotfiles
IGNORED_FILES=(README.md setup.sh install.sh dotdumper.sh post-merge-hook.sh)

function dot {
  git --git-dir=$HOME/.dot/ --work-tree=$HOME $@
}

if [ -d .dot ]; then
    echo "Already installed dotfiles"
    exit 1
fi


if [ ! -f /usr/bin/git ]; then
    echo "Please install git and try again."
    exit 1
fi


if [ -f /usr/bin/ssh ]; then
    DOT_URL=git@github.com:$REPO_NAME.git
else
    DOT_URL=https://github.com/$REPO_NAME.git
fi

echo "Cloning $DOT_URL."
git clone --bare $DOT_URL $HOME/.dot --recursive --quiet


mkdir -p .dot-backup

dot checkout &> /dev/null

if [ $? != '0' ]; then
    echo "Backing up pre-existing dot files on ~/.dot.backup."

    rm -rf .dot-backup
    touch .dot.log
    for file in $(dot checkout 2>&1 | egrep "^\s+" | awk '{$1=$1;print}')
    do
        back=".dot-backup/$file"
        mkdir -p $(dirname "$back")
        mv -v "$file" "$back" >> .dot.log
    done
    echo "Backup finished. Look into .dot.log."


fi

dot reset HEAD . > /dev/null
dot checkout . > /dev/null
dot config status.showUntrackedFiles no

echo "Dotfiles installed."

bash install.sh

# install hook for deleting useless file on git pull
cp ~/post-merge-hook.sh ~/.dot/hooks/post-merge

# ignore loop (remove files which don't belong to dotfiles)
echo "Removing useless files..."
rm -rf ${IGNORED_FILES[@]}
dot update-index --assume-unchanged ${IGNORED_FILES[@]}

source ~/.bashrc
