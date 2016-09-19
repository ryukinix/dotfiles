";         Everybody is connected
"; : - ) ( - :   : - ) ( - :   : - ) ( - :
"; ^        . .  ^        . .  ^        . .
"; |    ☺️    |   |    ☺️    |   |    ☺️    |
";. .        v  . .        v  . .        v
"; ( - : : - )   ( - : : - )   ( - : : - )
";               \    |    /
"; : - ) ( - :     \  |  /     : - ) ( - :
"; ^        . .      +-+       ^        . .
"; |    ☺️    | ------|☺️|------ |    ☺️    |
";. .        v       +-+      . .        v
"; ( - : : - )     /  |  \     ( - : : - )
";               /    |    \ 
"; : - ) ( - :   : - ) ( - :   : - ) ( - :
"; ^        . .  ^        . .  ^        . .
"; |    ☺️    |   |    ☺️    |   |    ☺️    |
";. .        v  . .        v  . .        v
"; ( - : : - )   ( - : : - )   ( - : : - )

" Manoel Machado
" ---------- VERY IMPORTANT -----------
" To install plugin the first time:
" > vim +PlugInstall +qall
" -------------------------------------

call plug#begin('~/.vim/plugged')

" Distraction Free Writting
Plug 'junegunn/goyo.vim'

" completion during typing
Plug 'neocomplcache'
" Right way to handle trailing-whitespace
Plug 'bronson/vim-trailing-whitespace'
" NERDTree
Plug 'scrooloose/nerdtree'
Plug 'Shougo/vimproc.vim'
" GIT
" show which line changed using git
Plug 'airblade/vim-gitgutter'
" Align code
Plug 'junegunn/vim-easy-align'

Plug 'wakatime/vim-wakatime'

call plug#end()

set nocompatible

" ###################
" ### Plugin conf ###
" ###################


" ----------------
"       GIT
" ----------------

" -- vim-gitgutter
highlight clear SignColumn
highlight SignColumn ctermbg=0
nmap gn <Plug>GitGutterNextHunk
nmap gN <Plug>GitGutterPrevHunk

" -----------------
"       THEME
" -----------------

" -- monokai

syntax enable
set background=dark
try
    colorscheme monokai
catch
endtry


" #####################
" ### Personal conf ###
" #####################

" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible
set bs=2		        " allow backspacing over everything in insert mode
set viminfo='20,\"50    " read/write a .viminfo file, don't store more
			            " than 50 lines of registers
set history=10000	    " keep 100000 lines of command line history
set ruler		        " show the cursor position all the time

set hlsearch " highlight searches


" move between splits
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" -- sudo save
cmap w!! w !sudo tee >/dev/null %

" Tabulation management
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
set autoindent
set smartindent
set cindent
set cinoptions=(0,u0,U0

" Easy align interactive
vnoremap <silent> <Enter> :EasyAlign<cr>

" ========
" Personal
" ========

" Python execute
autocmd FileType python nnoremap <buffer> <F8> :exec '!clear; python "%"' <cr>
" C execute
autocmd FileType c nnoremap <buffer> <F8> :exec '!clear; gcc "%" -o temp.out && ./temp.out && rm temp.out' <cr>
" Clojure execute
autocmd FileType clojure nnoremap <buffer> <F8> :exec '%Eval' <cr>
autocmd BufRead * nnoremap <buffer> <F5> :exec ':w' <cr>
