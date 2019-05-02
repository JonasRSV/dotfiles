
function s:customfiles(ArgLead, CmdLine, CursorPos)
  return systemlist("ls ~/.vim/templates")
endfunction

command -nargs=1 -complete=customlist,s:customfiles Template execute "0r ~/.vim/templates/" . <q-args>



