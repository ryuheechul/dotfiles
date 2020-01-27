" Color name (:help cterm-colors) or ANSI code
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240

" Goyo hooks
function! s:goyo_enter()
  if executable('tmux') && strlen($TMUX)
    silent !tmux set status off
    silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
  endif
  set noshowmode
  set noshowcmd
  set scrolloff=999
  Limelight
endfunction

function! s:goyo_leave()
  if executable('tmux') && strlen($TMUX)
    silent !tmux set status on
    silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
  endif
  set showmode
  set showcmd
  set scrolloff=5
  Limelight!
endfunction

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

  " register toggle goyo with <SPC> w z
  call SpaceVim#custom#SPC('nore', ['w', 'z'], ':Goyo', 'toggle Goyo', 1)
endfunction

function! myspacevim#after() abort
  " faster guide shows up
  set timeoutlen=500 

  " show listchars - https://medium.com/usevim/understanding-listchars-acb9e5a90854
  set nolist!

  " word wrap
  set wrap! breakindent!

  " customizing goyo
  let g:goyo_width = "50%"
  let g:goyo_height = "100%"
  let g:goyo_linenr = 1

  " register custom Goyo hooks
  autocmd! User GoyoEnter nested call <SID>goyo_enter()
  autocmd! User GoyoLeave nested call <SID>goyo_leave()
endfunction
