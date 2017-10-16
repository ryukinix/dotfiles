#!/bin/bash

cd ~/

source <(curl -s https://raw.githubusercontent.com/ryukinix/dotfiles/master/lib.sh) # sorry
source <(curl -s https://raw.githubusercontent.com/ryukinix/dotfiles/master/conf.sh) # sorry again

function clone_repo {
    if [ ! -f /usr/bin/git ]; then
        echo_error "error" "Please install git and try again."
        exit 1
    fi

    # if the host has ssh, so has ssh-add and if ssh-add
    # can connect to agent, probably is a proper ssh setup
    # clone with ssh so then.
    if [ -f /usr/bin/ssh-add && `ssh-add -L &> /dev/null` ]; then
        DOT_URL=git@github.com:$REPO_NAME.git
    else
        DOT_URL=https://github.com/$REPO_NAME.git
    fi

    echo_info "git" "Cloning $DOT_URL..."
    git clone --bare $DOT_URL $HOME/.dot --quiet
}



function backup_dotfiles {
    echo_info "backup" "Backing up pre-existing dot files on $BACKUP_DIR."

    rm -rf $BACKUP_DIR && mkdir -p $BACKUP_DIR
    # get conflict files (only if really exists)
    function conflict_files {
        dot checkout 2>&1 \
            | egrep "^\s+" \
            | awk '{$1=$1;print}' \
            | xargs -d '\n' -I{in} ls -d -b "{in}" 2> /dev/null \
            | cat
    }

    conflict_files | while read file; do
        dest="$BACKUP_DIR/$file"
        mkdir -p "$(dirname "$dest")"
        [[ -f "$file" ]] && mv -f "$file" "$dest"
    done

}

function install_dotfiles {
    if [ -d .dot ]; then
        echo_info "info" "Already installed dotfiles. do 'rm -rf ~/.dot' to a fresh install."
        exit 1
    fi

    clone_repo

    dot checkout &> /dev/null

    if [ $? != '0' ]; then
        backup_dotfiles
    fi

    dot reset HEAD . > /dev/null
    dot checkout . > /dev/null
    dot config status.showUntrackedFiles no

    echo_info "git" "Initializing submodules... just wait."
    # force deinit of git submodules first (avoid error when installing again)
    dot submodule deinit --all -q -f
    dot submodule update --init --recursive --quiet &&  echo "done." || echo "fail."
}

function post_install {
    # install hook for deleting useless file on git pull
    hooks=(~/.dot/hooks/post-checkout
           ~/.dot/hooks/post-rewrite
           ~/.dot/hooks/post-merge)
    cat conf.sh lib.sh post-hook | tee ${hooks[@]} 1> /dev/null
    chmod +x ${hooks[@]}
    source post-hook # execute post-hook (remove useless files)
}

# main installation
install_dotfiles
bash install.sh
post_install
echo_info "info" "Dotfiles installation finished."
