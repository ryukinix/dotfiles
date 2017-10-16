alias emacs='emacsclient -nw -a vim'
alias remacs='sudo rc-service emacs.lerax restart'
alias agenda='gcalcli agenda'
alias dotnet-build='msbuild'
alias sbcl='rlwrap sbcl'
alias lisp=sbcl
alias lain=lein
alias dic=sdcv

alias deadstar-bind-up='ssh -Y lerax@deadstar -t "x2x -north -to :0.0"'
alias deadstar-bind-left='ssh -Y lerax@deadstar -t "x2x -west -to :0.0"'
alias deadstar-bind-right='ssh -Y lerax@deadstar -t "x2x -east -to :0.0"'
alias deadstar-bind-down='ssh -Y lerax@deadstar -t "x2x -south -to :0.0"'
alias starfox='ssh lerax@starfox'
alias deadstar='ssh lerax@deadstar'

alias dot='/usr/bin/git --git-dir=$HOME/.dot/ --work-tree=$HOME'
alias dot-tig='GIT_DIR=$HOME/.dot/ tig'
