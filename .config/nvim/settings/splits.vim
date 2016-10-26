let g:dwm_map_keys=0

" DWM
" See: https://github.com/spolu/dwm.vim/blob/master/plugin/dwm.vim#L196-L224
nnoremap <C-x> <C-W>w
nnoremap <C-z> <C-W>W

if !hasmapto('<Plug>DWMNew')
  nmap <C-N> <Plug>DWMNew
endif

" Instead of C-c close window (DWM plugin), delete buffer
nnoremap <C-C> :bd<CR>

if !hasmapto('<Plug>DWMFocus')
  " Theres a trick here: when I hit C-Space,
  " activate focus by dwm plugin then call
  " vim-eighties resize
  function! FocusWindow()
    execute "normal \<Plug>DWMFocus"
    execute 'EightiesResize'
  endfunction

  :nnoremap <Space> :call FocusWindow()<CR>
endif

" Move between terminal splits
:nnoremap <C-h> <C-\><C-n><C-w>h
:nnoremap <C-j> <C-\><C-n><C-w>j
:nnoremap <C-k> <C-\><C-n><C-w>k
:nnoremap <C-l> <C-\><C-n><C-w>l

