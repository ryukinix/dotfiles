# install main packages
if [ -f /usr/bin/pacman ]; then
    sudo pacman -Sy \
         zsh \
         vim \
         conky \
         emacs --no-confirm

    chsh -s /bin/zsh
fi

# install prelude for emacs
function install-prelude {
    cd ~/.emacs.d/
    git config --unset core.bare
    git clone --bare git@github.com:bbatsov/prelude.git .git
    git reset
    cd ~/
}

# install vim Vundle to handle plugins
function install-vim-deps {
    mkdir -p ~/.vim/bundle/
    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
}

install-prelude
install-vim-deps
