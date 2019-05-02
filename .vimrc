set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'junegunn/fzf.vim'
Plugin 'junegunn/fzf'


Plugin 'neoclide/coc.nvim'

call vundle#end()            " required

" Language Server plugins

set completeopt=noinsert,menuone,noselect


set backupdir=/Users/jonval/.backups
set directory=/Users/jonval/.backups


let g:disable_vim_auto_close_plugin = 1
"Global sets
set laststatus=1
set statusline=%f\ %y

let g:netrw_altv=1
let g:netrw_preview   = 1
let g:netrw_liststyle = 3
let g:netrw_winsize   = 75

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

if has("nvim")
  nnoremap <C-space> <C-^>`"zz
  nnoremap <leader><C-space> :vertical sbp<CR>`"zz
else
  map <C-@> <C-^>`"zz
  map <leader><C-@> :vertical sbp<CR>`"zz
endif

inoremap (<CR> ()<Esc>i
inoremap {<CR> {<CR>}<Esc>O
inoremap {; {<CR>};<Esc>O
inoremap {, {<CR>},<Esc>O
inoremap [<CR> []<Esc>i
inoremap [; [];<Esc>i<Esc>i
inoremap [, [],<Esc>i<Esc>i

map <space> <leader>
map <C-b> :b 
map <leader><C-b> :vertical sb 
map <C-d> :bd<CR>
map <C-e> :Explore<CR>
map s /
map ö }
map ä {
map Y 0y$
map R :%s//



"stty -ixon IS NEEDED FOR C-s binding put in *rc
nnoremap gw :RgSearchWord<CR>
nnoremap <F2> :silent! Make<CR>

highlight QuickFixLine term=bold,underline cterm=bold,underline gui=bold,underline
highlight Folded guibg=#3a3c3f guifg=#c0c4ce

nnoremap <C-f> :FZF<CR>
nnoremap <C-g> :Rg<space>

nnoremap <C-n> :cn<Cr>
noremap  <C-p> :cp<Cr>

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


nnoremap <C-l> :CocList<CR>



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
