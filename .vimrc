set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'junegunn/fzf.vim'
Plugin 'junegunn/fzf'
Plugin 'majutsushi/tagbar'
Plugin 'xolox/vim-notes'
Plugin 'xolox/vim-misc'


call vundle#end()            " required

" Language Server plugins

set completeopt=noinsert,menuone,noselect


set backupdir=/home/jonas/.backups
set directory=/home/jonas/.backups


let g:disable_vim_auto_close_plugin = 1
"Global sets
set laststatus=1
set statusline=%f\ %y

set encoding=utf-8
set clipboard+=unnamed,unnamedplus
set nocompatible "Not sure what it does but people claim its useful"
set path=~/ "Path to root"
set mouse=a "enable mouse"

"Switching Buffers without saving
set hidden

"For Regexes
set magic

"Fix Searching
set hlsearch "Highligh search hits"
set incsearch "Not sure"

set sh=/bin/zsh "Shell to use"
" not sure set ef=e.err
set title "Enable Title on window"


set wildmenu "Autocompletion in commandline"
set wildmode=longest:full,full "How to autocomplete"
set wildignore+=*/target/*,*/.git/*,*/node_modules/* "What autocomplete ignores"
set tagstack "Enables Stack for tags"
set autoread "Not Sure But useful apparently"

set showmatch

"Default Indentation
set tabstop=2 "not sure"
set shiftwidth=2  "tab width"
set expandtab "Convert tab to spaces"
set ai "Keeps indentation from last line"

"autocmd BufReadPre,BufNewFile * let b:did_ftplugin = 1

set termguicolors
colo mycolo


let &t_8f = "\<Esc>[38:2:%lu:%lu:%lum"
let &t_8b = "\<Esc>[48:2:%lu:%lu:%lum"

syntax on
filetype plugin indent on

"if has("nvim")
"nnoremap <C-space>j :bp<CR>`"zz
"nnoremap <C-space>k :bn<CR>`"zz
"nnoremap <leader><C-space> :vertical sbp<CR>`"zz
"else
"nnoremap <C-@>j :bp<CR>`"zz
"nnoremap <C-@>k :bn<CR>`"zz
"map <leader><C-@> :vertical sbp<CR>`"zz
"endif

inoremap (<CR> ()<Esc>i
inoremap {<CR> {<CR>}<Esc>O
inoremap {; {<CR>};<Esc>O
inoremap {, {<CR>},<Esc>O
inoremap [<CR> []<Esc>i
inoremap [; [];<Esc>i<Esc>i
inoremap [, [],<Esc>i<Esc>i

map <space> <leader>
map <C-b> :Buffers<CR>
map s :Lines<CR>
map <C-d> :bd<CR>
map ö 7j
map ä 7k
map Y 0y$
map Z zz

" Toggle Vexplore with Ctrl-E
function! ToggleVExplorer()
  if exists("t:expl_buf_num")
    let expl_win_num = bufwinnr(t:expl_buf_num)
    if expl_win_num != -1
      let cur_win_nr = winnr()
      exec expl_win_num . 'wincmd w'
      close
      exec cur_win_nr . 'wincmd w'
      unlet t:expl_buf_num
    else
      unlet t:expl_buf_num
    endif
  else
    exec '1wincmd w'
    Vexplore
    let t:expl_buf_num = bufnr("%")
  endif
endfunction
let g:netrw_browse_split = 4
let g:netrw_altv = 1 
map <silent> <C-e> :call ToggleVExplorer()<CR>

"stty -ixon IS NEEDED FOR C-s binding put in *rc
nnoremap gw :RgSearchWord<CR>
nnoremap <F2> :silent! Make<CR>

highlight QuickFixLine term=bold,underline cterm=bold,underline gui=bold,underline
highlight Folded guibg=#3a3c3f guifg=#c0c4ce

nnoremap <C-f> :FZF<CR>
nnoremap <C-g> :Rg<space>

nnoremap <C-n> :cn<Cr>
noremap  <C-p> :cp<Cr>
nnoremap <C-h> H
nnoremap <C-m> M
nnoremap <C-l> L
nnoremap <C-space> zz
nnoremap F gg=G<C-o><C-o>zz

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <leader>rn <Plug>(coc-rename)


" Use K for show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Remap for format selected region
vmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)


command! Make execute "make " . expand("%") . " | redraw!"
command! RgSearchWord execute "Rg " . expand("<cword>") 


command! Vterm execute "vsplit |term"

au FileType netrw setl bufhidden=delete
au FileType python set equalprg=yapf
au FileType go set equalprg=gofmt
au FileType javascript set equalprg=standard\ --stdin\ --fix

augroup remember_folds
  autocmd!
  autocmd BufWinLeave *.py mkview
  autocmd BufWinLeave *.go mkview
  autocmd BufWinEnter *.py silent! loadview
  autocmd BufWinEnter *.go silent! loadview
augroup END

func BashColors(A, L, P)
  return "green\nnormal\nblue\nred\nhlred\nhlblue\nblink\nreset"
endfun

func WriteColor(color)
  echom "Hello"
  echom a:color
  echo "HI"
  if a:color == "green"
    call feedkeys('a\033[32m', 't') 
  elseif a:color == "normal"
    call feedkeys('a\033[39m', 't')
  elseif a:color == "blue"
    call feedkeys('a\033[34m', 't')
  elseif a:color == "red"
    call feedkeys('a\033[31m', 't')
  elseif a:color == "hlred"
    call feedkeys('a\033[41m', 't')
  elseif a:color == "hlblue"
    call feedkeys('a\033[44m', 't')
  elseif a:color == "bold"
    call feedkeys('a\033[1m', 't')
  elseif a:color == "reset"
    call feedkeys('a\033[0m', 't')
  else
    echo "Unknown Action :("
  endif
endfun

command! -nargs=1 -complete=custom,BashColors BashColor call WriteColor(<f-args>)



nnoremap tb :TagbarToggle<CR>
" Golang Tags
let g:tagbar_type_go = {
      \ 'ctagstype' : 'go',
      \ 'kinds'     : [
      \ 'p:package',
      \ 'i:imports:1',
      \ 'c:constants',
      \ 'v:variables',
      \ 't:types',
      \ 'n:interfaces',
      \ 'w:fields',
      \ 'e:embedded',
      \ 'm:methods',
      \ 'r:constructor',
      \ 'f:functions'
      \ ],
      \ 'sro' : '.',
      \ 'kind2scope' : {
      \ 't' : 'ctype',
      \ 'n' : 'ntype'
      \ },
      \ 'scope2kind' : {
      \ 'ctype' : 't',
      \ 'ntype' : 'n'
      \ },
      \ 'ctagsbin'  : 'gotags',
      \ 'ctagsargs' : '-sort -silent'
      \ }
