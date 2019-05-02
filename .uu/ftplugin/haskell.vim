
compiler ghc


function! HaskellMake()
  if glob("*.cabal") != ""
    set makeprg=stack\ install
    echom "Stack Installing"
  else
    echom "Running GHC"
    set makeprg=stack\ runghc\ %
  endif
    
  execute "make | cope | redraw!"
endfunction

command! Make call HaskellMake()

