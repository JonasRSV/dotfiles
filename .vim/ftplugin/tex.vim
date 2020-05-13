command! Runpdflatex execute "!pdflatex -interaction=nonstopmode " . expand("%")
command! TexBasic execute "0r ~/.vim/templates/tex.article"

"au! BufWritePost *.tex :silent! Runpdflatex 

compiler tex
set makeprg=pdflatex



