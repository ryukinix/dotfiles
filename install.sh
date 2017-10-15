PACKAGES=(zsh vim conky emacs tmux)
DEFAULT_SHELL=zsh


function install-packages {
# install main packages
if [ -f /usr/bin/pacman ]; then
    echo "Arch Linux based. Installing: ${PACKAGES[@]}."
    sudo pacman -Sy --needed ${PACKAGES[@]} && chsh -s $DEFAULT_SHELL
elif [ -f /usr/bin/apt-get ]; then
    echo "Debian based. Installing: ${PACKAGES[@]}."
    sudo apt-get update && \
         sudo apt-get install ${PACKAGES[@]} && \
         chsh -s $DEFAULT_SHELL
fi
}


# install prelude for emacs
function install-prelude {
    cd ~/.emacs.d/
    if [ ! -d .git ]; then
        git clone --bare https://github.com/bbatsov/prelude.git .git --quiet
        git config --unset core.bare
        git reset --hard @ --quiet
    fi
    cd ~/
}


# install vim Vundle to handle plugins
function install-vim-deps {
    mkdir -p ~/.vim/bundle/
    [[ -d ~/.vim/bundle/Vundle.vim/ ]] || git clone https://github.com/VundleVim/Vundle.vim.git \
                                                    ~/.vim/bundle/Vundle.vim --quiet
}

printf "Installing packages..."
install-packages && echo "done." || echo "fail or skipped."
printf "Installing prelude for emacs..."
install-prelude && echo "done." || echo "fail."
printf "Installing vim deps..."
install-vim-deps && echo "done." || echo "fail."
