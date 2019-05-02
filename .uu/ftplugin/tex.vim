command! Runpdflatex execute "!pdflatex -interaction=nonstopmode " . expand("%")
command! TexBasic execute "0r /Users/jonval/.vim/templates/tex.article"

"au! BufWritePost *.tex :silent! Runpdflatex 

compiler tex
set makeprg=pdflatex



