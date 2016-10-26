set background=dark
syntax enable
filetype plugin on

"let $NVIM_TUI_ENABLE_TRUE_COLOR=1
"let base16colorspace=256
"set t_Co=256
"set t_AB=^[[48;5;%dm
"set t_AF=^[[38;5;%dm

colorscheme onedark

:set number relativenumber
autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber
:set list

set shiftwidth=4
set listchars=tab:-\ ,eol:¬
highlight Normal ctermbg=none
highlight NonText ctermbg=none
highlight NonText ctermfg=5
highlight SpecialKey ctermfg=5

