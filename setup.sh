#!/bin/bash

cd ~/

source <(curl -s https://raw.githubusercontent.com/ryukinix/dotfiles/master/lib.sh) # sorry
source <(curl -s https://raw.githubusercontent.com/ryukinix/dotfiles/master/conf.sh) # sorry again

function clone-repo {
    if [ ! -f /usr/bin/git ]; then
        echo-error "error" "Please install git and try again."
        exit 1
    fi

    if [ -f /usr/bin/ssh ]; then
        DOT_URL=git@github.com:$REPO_NAME.git
    else
        DOT_URL=https://github.com/$REPO_NAME.git
    fi

    echo-info "git" "Cloning $DOT_URL..."
    git clone --bare $DOT_URL $HOME/.dot --quiet
}



function backup-dotfiles {
    echo-info "backup" "Backing up pre-existing dot files on $BACKUP_DIR."

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

function install-dotfiles {
    if [ -d .dot ]; then
        echo-info "info" "Already installed dotfiles. do 'rm -rf ~/.dot' to a fresh install."
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

    echo-info "git" "Initializing submodules... just wait."
    dot submodule update --init --recursive --quiet &&  echo "done." || echo "fail."
}

function post-install {
    # install hook for deleting useless file on git pull
    cat conf.sh lib.sh post-hook | tee ~/.dot/hooks/post-merge ~/.dot/hooks/post-rebase
    source post-hook # execute post-hook (remove useless files)
}

# main installation
install-dotfiles
bash install.sh
post-install
echo-info "info" "Dotfiles installation finished."
