export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
async_pyenv_init() {
    eval "$(pyenv init -)"
}
async_pyenv_virtualenv_init() {
    eval "$(pyenv virtualenv-init -)"
}
async_pyenv_init&
async_pyenv_virtualenv_init&
