" Color name (:help cterm-colors) or ANSI code
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240

function! s:lsp()
  let g:ale_linters = {
  \ 'reason': ['reason-language-server'],
  \}
  let g:ale_fixers = {
  \ 'reason': ['refmt'],
  \}
  let g:ale_lint_on_save = 1
  let g:ale_fix_on_save = 1

  let g:LanguageClient_serverCommands = {
  \ 'reason': ['reason-language-server']
  \ }
endfunction

function! s:providers()
  let g:python3_host_prog = expand('~/.asdf/shims/python3')
  let g:python_host_prog = expand('~/.asdf/shims/python2')
endfunction

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
  " hopefully drawing interfere key input much less
  set lazyredraw
  " for case insensitive search
  set ignorecase
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

  " disable by default for now
  " call kakoune#mimic_load()
endfunction

function! myspacevim#after() abort
  " faster guide shows up
  set timeoutlen=500

  " show listchars - https://medium.com/usevim/understanding-listchars-acb9e5a90854
  set nolist!

  " word wrap
  set wrap! breakindent!

  " customizing goyo
  let g:goyo_width = "60%"
  let g:goyo_height = "100%"
  let g:goyo_linenr = 1

  " register custom Goyo hooks
  autocmd! User GoyoEnter nested call <SID>goyo_enter()
  autocmd! User GoyoLeave nested call <SID>goyo_leave()

  " enable true color in terminal including tmux when it supports
  " https://github.com/tmux/tmux/issues/1246
  if $TERM_PROGRAM != 'Apple_Terminal' && ('+termguicolors')
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
  endif

  " add more options to eslint
  let g:neomake_javascript_eslint_args = g:neomake_javascript_eslint_args + ['--ignore-pattern', '!.eslintrc.js']

  call <SID>lsp()
  call <SID>providers()

  " set defx column to use icons and git
  call defx#custom#option('_', 'columns', 'git:mark:indent:icons:filename:type:size:time')
  Defx | Defx | wincmd p
endfunction
