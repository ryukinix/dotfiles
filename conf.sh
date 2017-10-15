REPO_NAME=ryukinix/dotfiles
IGNORED_FILES=(README.md # these files will be removed
               setup.sh
               lib.sh
               conf.sh
               install.sh
               post-hook)
BACKUP_DIR=.dot-backup/$(date -Ihours)
PACKAGES=(zsh vim conky emacs tmux)
DEFAULT_SHELL=zsh
