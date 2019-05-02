
if exists("g:disable_vim_auto_close_plugin") 
  finish
endif

let g:previouslyInsertedCharacter = ""

let g:vim_auto_close_dictionary = {
  \ '(': ')',
  \ '{': '}',
  \ '[': ']',
  \ '"': '"',
  \ "'": "'",
  \ }

function! Vim_Auto_Close_Plugin()
  if has_key(g:vim_auto_close_dictionary, g:previouslyInsertedCharacter)
    let a:close = g:vim_auto_close_dictionary[g:previouslyInsertedCharacter]
    execute "normal i" . a:close 
    let g:previouslyInsertedCharacter = ""
  endif
endfunction

function ToggleAutoClose() 
  if !exists("#vim_autoclose_aus#InsertCharPre")
      augroup vim_autoclose_aus
        autocmd InsertCharPre * let g:previouslyInsertedCharacter = v:char
        autocmd TextChangedI * call Vim_Auto_Close_Plugin()
      augroup END 
  else
    augroup vim_autoclose_aus
      autocmd!
    augroup END
  endif
endfunction

call ToggleAutoClose()
command ToggleAutoClose call ToggleAutoClose()
