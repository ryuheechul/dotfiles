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

" inspired by https://www.halcyon.hr/posts/automatic-dark-mode-switching-for-vim-and-terminal/
function! s:change_color_scheme(data)
  let time = trim(a:data)

  if time ==# "day"
    " prevent lagging
    if &background !=# "light"
      set background=light
    endif
  else
    " prevent lagging
    if &background !=# "dark"
      set background=dark
    endif
  endif
endfunction

function! s:sunshine()
  let cached = system('cat ~/.cache/sunshine-result')

  call s:change_color_scheme(cached)
endfunction

function! Sunshine(timer)
  call s:sunshine()
endfunction

function! s:auto_dark_mode_setup()
  " timer to check day/night every 30 minutes
  let timer = timer_start(1000 * 30 * 60, 'Sunshine', {'repeat': -1})

  " immediate timer to change theme accordingly
  " using timer rather than directly calling function to avoid glitch of broken redrawing
  let immediate_timer = timer_start(0, 'Sunshine')
endfunction

function! myspacevim#before() abort
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
  " hopefully drawing interfere key input much less
  set lazyredraw

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

  " set defx column to use icons and git
  call defx#custom#option('_', 'columns', 'git:mark:indent:icons:filename:type:size:time')
  Defx | Defx | wincmd p

  call s:auto_dark_mode_setup()
endfunction
