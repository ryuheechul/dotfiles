"This vimscript is an attempt to mimic (not perfectly) traversing words with selections like Kakoune editor (heavily inspired by it of course.)
"You can try this out simply by sourcing this file and calling a function.
"How-to in an order
"1. download this file into your computer
"2. open your vim and type below
"`:source [path/to/]kakoune.vim`
"3. call a function by typing below
"`:call kakoune#mimic_load()`
"4. now try pressing w,e,b to move around with selection
"5. enjoy!
"
"Limitations: somehow it doesn't work when your setting is vanila with vim but it works with neovim even with vanila setup. In some cases it works with vim, when you have some setups like using spacevim. This is quite mysterious to me and I will keep an update here when get to figure out.

function! s:selectwordfoward() abort
  normal! wevb
endfunction

function! s:selectwordfoward_e() abort
  call search('\S')
  normal! wvel
endfunction

function! s:selectwordbackward() abort
  " when cursor is at the beginning of the line
  " somehow it's not zero and it's one
  if col('.') == 1
  " or left side of cursor is whitespace
    \ || getline('.')[col('.')-1] =~ "\\s"
    " jump to none whitespace char
    call search('\S', 'b')
  else
    " otherwise move one char left
    normal! h
  endif

  " select and somehow I need to include h
  normal! vbh

  " somehow when the word is at the begging of the line,
  " I need this part to include first char
  if col('.') == 1
    normal! b
  endif
endfunction

function! s:isLineBafterA(a, b) abort
  " case 1
  " a: 1
  " b: 2
  " 1 < 2 == true
  " 2 is after 1
  "
  " case 2
  " a: 3
  " b: 2
  " 3 < 2 == false
  " 2 is not after 3
  "
  " case 3
  " a: 3
  " b: 3
  " 3 < 3 == false
  " 3 is not after 3
  return line(a:a) < line(a:b)
endfunction

function! s:isColumnBafterA(a, b) abort
  return col(a:a) < col(a:b)
endfunction

function! s:isCursorBafterA(a, b) abort
  if s:isLineBafterA(a:a, a:b)
    return 1
  end
  return s:isColumnBafterA(a:a, a:b)
endfunction

function! s:visualW() abort
  if s:isCursorBafterA('v', '.')
    return "W"
  else
    return "oW"
  endif
endfunction

function! kakoune#mimic_load() abort
  " cancel selection for basic navigating in v visual mode
  vnoremap <expr><silent>j mode() == 'v' ? ':<C-u>normal! j<CR>' : 'j'
  vnoremap <expr><silent>k mode() == 'v' ? ':<C-u>normal! k<CR>' : 'k'
  vnoremap <expr><silent>h mode() == 'v' ? ':<C-u>normal! h<CR>' : 'h'
  vnoremap <expr><silent>l mode() == 'v' ? ':<C-u>normal! l<CR>' : 'l'

  " Kakoune like selection navigating
  " w
  nnoremap <silent>w :<C-u>call <SID>selectwordfoward()<CR>
  vnoremap <silent>w :<C-u>call <SID>selectwordfoward()<CR>
  " W
  vnoremap <expr><silent>W <SID>visualW()
  " e
  nnoremap <silent>e :<C-u>call <SID>selectwordfoward_e()<CR>
  vnoremap <silent>e :<C-u>call <SID>selectwordfoward_e()<CR>
  " b
  nnoremap <silent>b :<C-u>call <SID>selectwordbackward()<CR>
  vnoremap <silent>b :<C-u>call <SID>selectwordbackward()<CR>

  " allow inserting while visual mode
  " vnoremap <silent>i :<C-U>normal! i<CR>
  vnoremap <silent>i <Esc>i
  vnoremap <silent>a <Esc>a
  vnoremap <silent>A <Esc>A

  " x/X to delete just a char so use d to delete the selection
  vnoremap <silent>x <Esc>x
  vnoremap <silent>X <Esc>X
endfunction

