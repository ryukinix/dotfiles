export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
pyenv_init() {
    eval "$(pyenv init - -zsh --path --no-rehash)"
}
pyenv_init
