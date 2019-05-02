
if !exists("g:vim_rerun_data_path")
  let g:vim_rerun_data_path = "/tmp/vim_rerun_cache"
endif

if !filereadable(g:vim_rerun_data_path)
  call system("touch /tmp/vim_rerun_cache")

  let g:vim_rerun_data_path = "/tmp/vim_rerun_cache"

  echom "created /tmp/vim_rerun_cache, because couldnt find provided data path"
endif

if !exists("g:vim_rerun_favorites")
  let g:vim_rerun_favorites = ["make"]
endif


function! Vim_rerun()
  execute g:vim_most_recently_ran_command
endfunction

function! Vim_rerunHistory(num)
  let history = s:rerun_cache("", "", "")

  execute history[-1 * (a:num + 1)]
endfunction



function! s:rerun_cache(Arglead, CommandLine, CursorPos)
  return g:vim_rerun_favorites + readfile(g:vim_rerun_data_path)  
endfunction


function! Vim_rerun_run(recent_command)
  echo a:recent_command

  let g:vim_most_recently_ran_command = a:recent_command
  let cache = readfile(g:vim_rerun_data_path)

  call writefile(add(cache, a:recent_command), g:vim_rerun_data_path) 
      
  call Vim_rerun()
endfunction


let g:vim_most_recently_ran_command = s:rerun_cache("", "", "")[-1]

function ClearCache()
  call system("rm -rf " . g:vim_rerun_data_path . " && touch " . g:vim_rerun_data_path)
endfunction

function! s:get_favorites(Arglead, CommandLine, CursorPos)
  return g:vim_rerun_favorites
endfunction

command! RunClearCache call ClearCache()
command! -nargs=1 -complete=customlist,s:rerun_cache Run execute Vim_rerun_run(<q-args>)

