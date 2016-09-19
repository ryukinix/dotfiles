"
"                          ███████████████████████████
"                          ███████▀▀▀░░░░░░░▀▀▀███████
"                          ████▀░░░░░░░░░░░░░░░░░▀████
"                          ███│░░░░░░░░░░░░░░░░░░░│███
"                          ██▌│░░░░░░░░░░░░░░░░░░░│▐██
"                          ██░└┐░░░░░░░░░░░░░░░░░┌┘░██
"                          ██░░└┐░░░░░░░░░░░░░░░┌┘░░██
"                          ██░░┌┘▄▄▄▄▄░░░░░▄▄▄▄▄└┐░░██
"                          ██▌░│██████▌░░░▐██████│░▐██
"                          ███░│▐███▀▀░░▄░░▀▀███▌│░███
"                          ██▀─┘░░░░░░░▐█▌░░░░░░░└─▀██
"                          ██▄░░░▄▄▄▓░░▀█▀░░▓▄▄▄░░░▄██
"                          ████▄─┘██▌░░░░░░░▐██└─▄████
"                          █████░░▐█─┬┬┬┬┬┬┬─█▌░░█████
"                          ████▌░░░▀┬┼┼┼┼┼┼┼┬▀░░░▐████
"                          █████▄░░░└┴┴┴┴┴┴┴┘░░░▄█████
"                          ███████▄░░░░░░░░░░░▄███████
"                          ██████████▄▄▄▄▄▄▄██████████
"                          ███████████████████████████
"
"   You are about to experience a potent dosage of Vim. Watch your steps.
"
"                ╔══════════════════════════════════════════╗
"                ║           ⎋ HERE BE VIMPIRES ⎋           ║
"                ╚══════════════════════════════════════════╝

colorscheme Tomorrow-Night-Eighties " Tomorrow Night Eighties theme
set background=dark
set number                          " View number lines
set expandtab                       " Always use spaces to indent anyway
set shiftwidth=4                    " Indent using 4 spaces
set tabstop=4                       " A tab character is 4 spaces wide
set shortmess+=afilmnrxoOtT         " Less vim verbosity
set hidden

let mapleader=","

set listchars=eol:¬,tab:»\ ,trail:~,extends:»,precedes:«

call plug#begin('~/.vim/plugged')


" Writing
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }
Plug 'Chiel92/vim-autoformat'
Plug 'ntpeters/vim-better-whitespace'
Plug 'wakatime/vim-wakatime'

" Interface
Plug 'mbbill/undotree', {'on': 'UndotreeToggle'}
Plug 'vim-airline/vim-airline' | Plug 'vim-airline/vim-airline-themes'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' } |
            \ Plug 'ryanoasis/vim-devicons'
Plug 'majutsushi/tagbar'
Plug 'edkolev/tmuxline.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'jeetsukumaran/vim-buffergator'

" Navigation
Plug 'justinmk/vim-sneak'
Plug 'terryma/vim-multiple-cursors'


call plug#end()

" Airline statusbar
let g:airline_theme='raven'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'

" vim-slime with tmux:
let g:slime_target = "tmux"
let g:slime_python_ipython = 1
let g:slime_dont_ask_default = 1
let g:slime_default_config = {"socket_name": "default", "target_pane": "1"}

" Sneak
let g:sneak#streak = 1

" Deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#clang#libclang_path = '/usr/lib/libclang.so'
let g:deoplete#sources#clang#clang_header = '/usr/lib/clang/'
let g:deoplete#sources#clang#std#c = 'c11'

let g:python_doc = 1
let g:pymode_python = 'python3'

" Dasht
" Search API docs for query you type in:
nnoremap <Leader>k :Dasht<Space>

" Search API docs for word under cursor:
nnoremap <silent> <Leader>K :call Dasht([expand('<cWORD>'), expand('<cword>')])<Return>

" Search API docs for the selected text:
vnoremap <silent> <Leader>K y:<C-U>call Dasht(getreg(0))<Return>

" Signify
let g:signify_vcs_list = [ 'git', 'hg']
let g:signify_sign_add               = '+'
let g:signify_sign_delete            = '-'
let g:signify_sign_delete_first_line = '-'
let g:signify_sign_change            = '.'
let g:signify_sign_changedelete = g:signify_sign_change

" NERDTree
let g:NERDTreeIndicatorMapCustom = {
            \ "Modified"  : "✹",
            \ "Staged"    : "✚",
            \ "Untracked" : "✭",
            \ "Renamed"   : "➜",
            \ "Unmerged"  : "═",
            \ "Deleted"   : "✖",
            \ "Dirty"     : "✗",
            \ "Clean"     : "✔︎",
            \ "Unknown"   : "?"
            \ }


if has("persistent_undo")
    set undodir=~/.undodir/
    set undofile
endif

augroup load_on_insert
    autocmd!
    autocmd InsertEnter * call plug#load('vim-fugitive') |
                \autocmd! load_on_insert
augroup END

nmap <F4> :NERDTreeToggle<CR>
nmap <F5> :Neomake<CR>
nmap <F9> :TagbarToggle<CR>
nmap <F10> :FZF!<CR>
nmap <F2> :w<CR>

let g:buffergator_viewport_split_policy = 'R'
" Go to the previous buffer open
nmap <leader>jj :BuffergatorMruCyclePrev<cr>

" Go to the next buffer open
nmap <leader>kk :BuffergatorMruCycleNext<cr>

" View the entire list of buffers open
nmap <leader>bl :BuffergatorOpen<cr>

" Shared bindings from Solution #1 from earlier
nmap <leader>T :enew<cr>
nmap <leader>bq :bp <BAR> bd #<cr>

au FileType xml set equalprg=xmllint\ --format\ --recover\ -\ 2>/dev/null
au FileType h,c,cpp set equalprg=astyle\ --style=allman\ --indent=spaces=4\
            \ --align-pointer=type\ --align-reference=name\ --attach-namespaces\
            \ --attach-classes\ --attach-inlines\ --attach-extern-c\
            \ --indent-cases\ --indent-switches\ --break-blocks=all\
            \ --pad-oper\ --pad-header\ --close-templates\ --add-brackets\
            \ --remove-comment-prefix\ --convert-tabs\ --max-code-length=89\
            \ --break-after-logical\ --max-instatement-indent=40\
            \ --indent-preproc-define\ --indent-preproc-cond

" My keybindings
if filereadable(expand("~/.vimrc.local"))
  source ~/.vimrc.local
endif
