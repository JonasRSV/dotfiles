command! RecompileSVG execute "!rsvg-convert -h 128 icon.svg > icon.png"

au! BufWritePost *.svg :silent! RecompileSVG
