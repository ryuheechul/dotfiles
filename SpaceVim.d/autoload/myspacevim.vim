function! myspacevim#before() abort
  " watch file changes and auto reload
  " > https://stackoverflow.com/questions/2490227/how-does-vims-autoread-work/20418591
  set autoread
  au CursorHold,CursorHoldI * checktime
  au FocusGained,BufEnter * checktime

  " change default search tool for rg
  let profile = SpaceVim#mapping#search#getprofile('rg')
  let default_opt = profile.default_opts " + ['--no-ignore-vcs']
  call SpaceVim#mapping#search#profile({'rg' : {'default_opts' : default_opt}})

  " four different char for different indentation depth
  let g:indentLine_char_list = ['|', '¦', '┆', '┊']
endfunction

function! myspacevim#after() abort
  " faster guide shows up
  set timeoutlen=500 

  " show listchars - https://medium.com/usevim/understanding-listchars-acb9e5a90854
  setlocal nolist!

  " word wrap
  setlocal wrap! breakindent!
endfunction
