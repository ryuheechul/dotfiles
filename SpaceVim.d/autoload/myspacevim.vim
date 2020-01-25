function! myspacevim#before() abort
  " watch file changes and auto reload
  set autoread
  au CursorHold,CursorHoldI * checktime
  au FocusGained,BufEnter * checktime
  " https://stackoverflow.com/questions/2490227/how-does-vims-autoread-work/20418591
endfunction

function! myspacevim#after() abort
endfunction
