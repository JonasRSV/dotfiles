
if exists("g:disable_vim_preview")
  finish
endif

command ToggleHTMLPreview call ToggleHTMLPreview()

if !executable("chrome-cli")
  echom "vim-preview is dependent on chrome-cli, please install chrome cli and put in $PATH"
  echom "Mac: brew install chrome-cli"
  finish
endif

let g:vim_preview_session_id = -1

au Filetype html execute "let g:vim_preview_html_path = expand('%:p')"

function! UpdateHtml()

  if (index(split(system("chrome-cli list tabs")), "[" .  g:vim_preview_session_id . "]") < 0)
    let session_info = system("chrome-cli open " . "file://" . g:vim_preview_html_path)
    let g:vim_preview_session_id = split(session_info)[1]
  else 
    silent call system("chrome-cli reload -t " . g:vim_preview_session_id)
  endif
endfunction

function ToggleHTMLPreview() 
  if !exists("#vim_preview_html_events#BufWritePost")
    augroup vim_preview_html_events
      au BufWritePost *.html call UpdateHtml()
      au BufWritePost *.css call UpdateHtml()
      au BufWritePost *.js call UpdateHtml()
    augroup END
  else 
    augroup vim_preview_html_events
      autocmd!
    augroup END
  endif
endfunction


