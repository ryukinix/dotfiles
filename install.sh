# install main packages
if [ -f /usr/bin/pacman ]; then
    sudo pacman -Sy \
         zsh \
         vim \
         conky \
         emacs && chsh -s /bin/zsh
fi

# install prelude for emacs
function install-prelude {
    cd ~/.emacs.d/
    git clone --bare https://github.com/bbatsov/prelude.git .git --quiet
    git config --unset core.bare
    git reset --hard @
    cd ~/
}

# install vim Vundle to handle plugins
function install-vim-deps {
    mkdir -p ~/.vim/bundle/
    git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim --quiet
}

echo "Installing prelude for emacs..."
install-prelude
echo "Installing vim deps..."
install-vim-deps
