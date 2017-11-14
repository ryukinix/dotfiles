#!/bin/bash
source lib.sh
source conf.sh

function install_packages {
    # if no sudo installed, just evaluate the expression
    if [ ! `command_exists sudo` ]; then
        # creating a alias don't will work
        function sudo {
            eval "$@"
        }
    fi

    # install main packages
    if [ `command_exists pacman` ]; then
        echo_info info "Arch Linux based: using pacman."
        sudo pacman -Sy --needed ${PACKAGES[@]}
    elif [ `command_exists apt-get` ]; then
        echo_info info "Debian based: using apt-get."
        sudo apt-get update &&  sudo apt-get install ${PACKAGES[@]}
    fi

    [[ -f $DEFAULT_SHELL ]] && chsh -s $DEFAULT_SHELL
}


# install prelude for emacs
function install_prelude {
    cd ~/.emacs.d/
    if [ ! -d .git ]; then
        git clone --bare https://github.com/bbatsov/prelude.git .git --quiet
        git config --unset core.bare
        git reset --hard @ --quiet
    fi
    cd ~/
}


# install vim Vundle to handle plugins
function install_vim_deps {
    mkdir -p ~/.vim/bundle/
    [[ -d ~/.vim/bundle/Vundle.vim/ ]] || git clone https://github.com/VundleVim/Vundle.vim.git \
                                                    ~/.vim/bundle/Vundle.vim --quiet
}

echo_info installing "system packages: ${PACKAGES[@]}"
install_packages
echo_info installing "prelude for emacs..."
install_prelude
echo_info installing "vim deps..."
install_vim_deps
