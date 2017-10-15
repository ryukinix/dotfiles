#!/bin/bash

cd ~/

REPO_NAME=ryukinix/dotfiles
IGNORED_FILES=(README.md setup.sh install.sh dotdumper.sh post-merge-hook.sh)

if [ -d .dot ]; then
    echo "Already installed dotfiles"
    exit 1
fi

if [ ! -f /usr/bin/git ]; then
    echo "Please install git and try again."
    exit 1
fi


if [ ! -f /usr/bin/ssh ]; then
    DOT_URL=git@github.com:$REPO_NAME.git
else
    DOT_URL=https://github.com/$REPO_NAME.git
fi


git clone --bare $DOT_URL $HOME/.dot --recursive --quiet

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



echo "Dotfiles installed."

bash install.sh

# install hook for deleting useless file on git pull
cp -v ~/post-merge-hook.sh ~/.dot/hooks/post-merge

# ignore loop (remove files which don't belong to dotfiles)
echo "Removing useless files..."
rm -rfv ${IGNORED_FILES[@]}
dot update-index --assume-unchanged ${IGNORED_FILES[@]}
