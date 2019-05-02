compiler gcc
"set makeprg=g++\ -std=c++11\ %\ -o\ %<\ &&\ ./%<   

command! CppSimple execute "0r /Users/jonval/.vim/templates/c.cpp"
