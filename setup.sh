#!/bin/bash

cd ~/

source <(curl -s https://raw.githubusercontent.com/ryukinix/dotfiles/master/lib.sh) # sorry
source <(curl -s https://raw.githubusercontent.com/ryukinix/dotfiles/master/conf.sh) # sorry again

function clone_repo {
    if [[ ! `command_exists git` ]]; then
        echo_error "error" "Please install git and try again."
        exit 1
    fi

    # if the host has ssh, so has ssh-add and if ssh-add
    # can connect to agent, probably is a proper ssh setup
    # clone with ssh so then.
    if [[ -f /usr/bin/ssh-add && `ssh-add -L &> /dev/null` ]]; then
        DOT_URL=git@github.com:$REPO_NAME.git
    else
        DOT_URL=https://github.com/$REPO_NAME.git
    fi

    echo_info "git" "Cloning $DOT_URL..."
    git clone --bare $DOT_URL $HOME/.dot --quiet
}

function list_submodules {
    dot reset -q @ .gitmodules; dot checkout -q .gitmodules;
    dot config --file .gitmodules --get-regexp path | awk '{ print $2 }'
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

    # remove git submodules HACK HACK HACK
    list_submodules | exists | xargs rm -rf


}

function install_dotfiles {
    if [ -d .dot ]; then
        echo_info "info" "Already installed dotfiles. Call before 'rm -rf ~/.dot' to a fresh install."
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
    dot submodule update --init --recursive --force --quiet
    if [[ $? != '0' ]]; then
        echo_error "git" "probably some submodule failed to fetch, fallback all to master."
        list_submodules | while read f; do
            echo_info "git" "set submodule $f to master."
            cd $f
            git reset -q master --hard;
            git submodule update --init --recursive --force --quiet
            cd $OLDPWD
        done
    fi

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
echo_info "post_install" "Removing ${IGNORED_FILES[@]} and installing hooks."
post_install
echo_info "info" "Dotfiles installation finished."
