#
# Executes commands at login pre-zshrc.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

#
# Browser
#

if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

#
# Editors
#

export EDITOR='emacsclient -nw'
export VISUAL='emacsclient -nw'
export PAGER='less'

#
# Language
#

if [[ -z "$LANG" ]]; then
  export LANG='en_US.UTF-8'
fi

#
# Paths
#

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# Set the list of directories that cd searches.
# cdpath=(
#   $cdpath
# )

# Set the list of directories that Zsh searches for programs.
path=(
  /usr/local/{bin,sbin}
  ~/go/bin
  ~/.local/bin
  ~/.cargo/bin
  ~/.ghcup/bin
  ~/.roswell/bin
  ~/Android/Sdk/tools/
  ~/Android/Sdk/tools/bin/
  ~/Android/Sdk/platform-tools/
  ~/.fzf/bin/
  ~/.git-fuzzy/bin
  ~/.emacs.d.doomemacs/bin
  ~/.local/share/flatpak/exports/share
  ~/.local/share/coursier/bin
  ~/.qlot/bin
  /usr/local/texlive/2025/bin/x86_64-linux
  $path
)

#
# Less
#

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
# Try both `lesspipe` and `lesspipe.sh` as either might exist on a system.
if (( $#commands[(i)lesspipe(|.sh)] )); then
  export LESSOPEN="| /usr/bin/env $commands[(i)lesspipe(|.sh)] %s 2>&-"
fi

# >>> JVM installed by coursier >>>
export JAVA_HOME="/home/manoel-neto/.cache/coursier/arc/https/github.com/adoptium/temurin17-binaries/releases/download/jdk-17.0.15%252B6/OpenJDK17U-jdk_x64_linux_hotspot_17.0.15_6.tar.gz/jdk-17.0.15+6"
export PATH="$PATH:/home/manoel-neto/.cache/coursier/arc/https/github.com/adoptium/temurin17-binaries/releases/download/jdk-17.0.15%252B6/OpenJDK17U-jdk_x64_linux_hotspot_17.0.15_6.tar.gz/jdk-17.0.15+6/bin"
# <<< JVM installed by coursier <<<
