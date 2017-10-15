#!/bin/bash

cd ~/

REPO_NAME=ryukinix/dotfiles
IGNORED_FILES=(README.md setup.sh install.sh dotdumper.sh post-merge-hook.sh)
BACKUP_DIR=.dot-backup/$(date -Ihours)

function dot {
  git --git-dir=$HOME/.dot/ --work-tree=$HOME $@
}

function clone-repo {
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

}


function backup-dotfiles {
    echo "Backing up pre-existing dot files on $BACKUP_DIR."

    rm -rf $BACKUP_DIR && mkdir -p $BACKUP_DIR
    # get conflict files (only if really exists)
    function conflict-files {
        dot checkout 2>&1 \
            | egrep "^\s+" \
            | awk '{$1=$1;print}' \
            | xargs -d '\n' -I{in} ls -d -b "{in}" 2> /dev/null \
            | cat
    }

    conflict-files | while read file; do
        dest="$BACKUP_DIR/$file"
        mkdir -p "$(dirname "$dest")"
        [[ -f "$file" ]] && mv -f "$file" "$dest"
    done
}

if [ -d .dot ]; then
    echo "Already installed dotfiles"
    exit 1
fi

clone-repo


dot checkout &> /dev/null

if [ $? != '0' ]; then
    backup-dotfiles
fi

dot reset HEAD . > /dev/null
dot checkout . > /dev/null
dot config status.showUntrackedFiles no

echo "Dotfiles installed."

bash install.sh

# install hook for deleting useless file on git pull
cp ~/post-merge-hook.sh ~/.dot/hooks/post-merge

# ignore loop (remove files which don't belong to dotfiles)
rm -rf ${IGNORED_FILES[@]}
dot update-index --assume-unchanged ${IGNORED_FILES[@]}

source ~/.bashrc
