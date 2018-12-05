# Borrowed from:
# https://github.com/sorin-ionescu/prezto/blob/master/modules/python/init.zsh

# Why not using directly this python module on zprezto since I use it?
# BECAUSE IT'S TOO SLOW WITHOUT ANY REASON.

#
# Enables local Python package installation.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#   Sebastian Wiesner <lunaryorn@googlemail.com>
#   Patrick Bos <egpbos@gmail.com>
#


function _python-workon-cwd {
    # Check if this is a Git repo
    local GIT_REPO_ROOT=""
    local GIT_TOPLEVEL="$(git rev-parse --show-toplevel 2> /dev/null)"
    if [[ $? == 0 ]]; then
        GIT_REPO_ROOT="$GIT_TOPLEVEL"
    fi
    # Get absolute path, resolving symlinks
    local PROJECT_ROOT="${PWD:A}"
    while [[ "$PROJECT_ROOT" != "/" && ! -e "$PROJECT_ROOT/.venv" \
                 && ! -d "$PROJECT_ROOT/.git"  && "$PROJECT_ROOT" != "$GIT_REPO_ROOT" ]]; do
        PROJECT_ROOT="${PROJECT_ROOT:h}"
    done
    if [[ "$PROJECT_ROOT" == "/" ]]; then
        PROJECT_ROOT="."
    fi
    # Check for virtualenv name override
    local ENV_NAME=""
    if [[ -f "$PROJECT_ROOT/.venv" ]]; then
        ENV_NAME="$(cat "$PROJECT_ROOT/.venv")"
    elif [[ -f "$PROJECT_ROOT/.venv/bin/activate" ]]; then
        ENV_NAME="$PROJECT_ROOT/.venv"
    elif [[ "$PROJECT_ROOT" != "." ]]; then
        ENV_NAME="${PROJECT_ROOT:t}"
    fi
    if [[ -n $CD_VIRTUAL_ENV && "$ENV_NAME" != "$CD_VIRTUAL_ENV" ]]; then
        # We've just left the repo, deactivate the environment
        # Note: this only happens if the virtualenv was activated automatically
        deactivate && unset CD_VIRTUAL_ENV
    fi
    if [[ "$ENV_NAME" != "" ]]; then
        # Activate the environment only if it is not already active
        if [[ "$VIRTUAL_ENV" != "$WORKON_HOME/$ENV_NAME" ]]; then
            if [[ -e "$ENV_NAME/bin/activate" ]]; then
                source $ENV_NAME/bin/activate && export CD_VIRTUAL_ENV="$ENV_NAME"
            fi
        fi

    fi
}


# Load auto workon cwd hook
# Auto workon when changing directory
autoload -Uz add-zsh-hook
add-zsh-hook chpwd _python-workon-cwd
