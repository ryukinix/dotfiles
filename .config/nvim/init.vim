set nocompatible

call plug#begin('~/.vim/plugged')
for plug in split(globpath('~/.config/nvim/plugs', '*.plug'), '\n')
  exe 'source' plug
endfor
call plug#end()

set hidden

for setting in split(globpath('~/.config/nvim/settings', '*.vim'), '\n')
  exe 'source' setting
endfor

" Exit insert mode
:imap ii <Esc>

" Project specific `.nvimrc`
:set exrc
set secure

" Let me quickly refresh my settings without having to
" restart Neovim
:nnoremap <S-R> :so ~/.config/nvim/init.vim<CR>

