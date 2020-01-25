function! myspacevim#before() abort
  let g:webdevicons_enable_vimfiler = 1
endfunction

function! myspacevim#after() abort
  set autoread
  au FocusGained,BufEnter * :checktime
  let g:webdevicons_enable_vimfiler = 1
endfunction
