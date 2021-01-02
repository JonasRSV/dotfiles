set rtp+=~/.vim/bundle/Vundle.vim


call vundle#begin()
" alternatively, pass a path where Vundle should install plugins

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Utilities
Plugin 'scrooloose/nerdcommenter'
Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'


" Code forces
Plugin 'gabrielsimoes/cfparser.vim'

" Completion
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'neoclide/coc.nvim'
Plugin 'neovim/nvim-lspconfig'
Plugin 'rust-lang/rust.vim'
Plugin 'cespare/vim-toml'

" FZF + LSP
Plugin 'ojroques/nvim-lspfuzzy'

" Niceities
Plugin 'ryanoasis/vim-devicons'

" Navigation
Plugin 'majutsushi/tagbar'
Plugin 'ms-jpq/chadtree'

" Front-End
Plugin 'mattn/emmet-vim'

" Note taking
Plugin 'NLKNguyen/papercolor-theme'
Plugin 'junegunn/goyo.vim'

"Syntax highlight
Plugin 'uarun/vim-protobuf'
Plugin 'cakebaker/scss-syntax.vim'
Plugin 'chr4/nginx.vim'

Plugin 'evanleck/vim-svelte'

" Git 
Plugin 'tpope/vim-fugitive'
call vundle#end()            " required


lua vim.api.nvim_set_var("chadtree_settings", { sort_by = {"is_folder", "fname"} })

let g:python3_host_prog = '/usr/bin/python3.8'
set completeopt=noinsert,menuone,noselect



"autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')

let g:loaded_fzf_vim = 1

lua << EOF
require'lspconfig'.rust_analyzer.setup{}
require('lspfuzzy').setup {}

local nvim_lsp = require'lspconfig'
  -- Disable Diagnostcs globally
  vim.lsp.callbacks["textDocument/publishDiagnostics"] = function() end

EOF

nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> K  <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>


let g:completion_enable_snippet = 'UltiSnips'
let g:UltiSnipsJumpForwardTrigger	= "<C-m>"
let g:UltiSnipsRemoveSelectModeMappings = 0


set backupdir=/home/jonas/.backups
set directory=/home/jonas/.backups


let g:disable_vim_auto_close_plugin = 1
"Global sets
set laststatus=0

set encoding=utf-8
set guifont=DroidSansMono\ Nerd\ Font\ Complete\ Mono\ 14
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
set nu

" window splits are automatically on right now
set splitright

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

nnoremap <C-space> :CocAction<CR>
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

inoremap (<CR> ()<Esc>i
inoremap {<CR> {<CR>}<Esc>O
inoremap {; {<CR>};<Esc>O
inoremap {, {<CR>},<Esc>O
inoremap [<CR> []<Esc>i
inoremap [; [];<Esc>i<Esc>i
inoremap [, [],<Esc>i<Esc>i

nnoremap j gj
nnoremap k gk

nnoremap <C-w> <C-w>w
nnoremap <C-e> :CHADopen<CR>

map <space> <leader>
map <C-c> :bd<CR>
map ö 7j
map ä 7k
map Y 0y$
map Z zz

nnoremap L Lzz
nnoremap H Hzz

" For syncing clipboard on unix systems with xclip
"map <C-y> yy \| :call system("xclip -selection clipboard -in", @0)<CR>
"map <C-p> :r !xclip -selection clipboard -o<CR>


"stty -ixon IS NEEDED FOR C-s binding put in *rc
nnoremap gw :SearchWordWithRipGrep<CR>


highlight StatusLine guibg=#00000 guifg=#b8ff73
highlight QuickFixLine term=bold,underline cterm=bold,underline gui=bold,underline
highlight Folded guibg=#3a3c3f guifg=#c0c4ce
hi FoldColumn guifg=#00000 guibg=#00000
hi SignColumn guifg=#00000 guibg=#00000


nnoremap <C-f> :call Fzf_dev()<CR>
nnoremap <C-g> :Rg<CR>
nnoremap tb :TagbarToggle<CR>

nnoremap F gg=G<C-o><C-o>zz
nnoremap <C-j> :cnext<CR>
nnoremap <C-k> :cprev<CR>


command! Make execute "make " . expand("%") . " | redraw! | vertical cope | vertical resize 100 | wincmd p"
command! SearchWordWithRipGrep execute "Rg " . expand("<cword>") 


au BufRead,BufNewFile *.journal set filetype=journal

au FileType netrw setl bufhidden=delete
au FileType go setlocal equalprg=gofmt
au FileType javascript setlocal equalprg=js-beautify\ --stdin
au FileType haskell setlocal equalprg=stylish-haskell
au FileType cpp setlocal equalprg=clang-format
au FileType json setlocal equalprg=js-beautify

au FileType python setlocal makeprg=python3\ %
au FileType python setlocal equalprg=yapf
au FileType python compiler python
au FileType python nnoremap <F10> :silent exec "!python3 %"<CR>
au FileType html nnoremap <buffer> <leader><F10> :!xdg-open %<CR>

" Emmet only for html / css
let g:user_emmet_install_global = 0
autocmd FileType html,css,mail EmmetInstall


func RenderTex()
  silent! call system("latexmk -pdf ")
endfun

func WritingMode()
  setlocal statusline=\ 
  setlocal nonu
  setlocal nornu
  setlocal colorcolumn=
  setlocal laststatus=0
  hi FoldColumn guifg=#00000 guibg=#00000
  hi SignColumn guifg=#00000 guibg=#00000
  set wm=20
  set spell
endfun

augroup latex 
  autocmd!
  au BufWritePost *.tex call RenderTex()
  au FileType tex call WritingMode()
augroup END

augroup remember_folds
  autocmd!
  autocmd BufWinLeave *.py mkview
  autocmd BufWinLeave *.go mkview
  autocmd BufWinEnter *.py silent! loadview
  autocmd BufWinEnter *.go silent! loadview
augroup END

" Floating Window Stuff

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case -- '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(), <bang>0)

" general
let g:fzf_layout = { 'window': 'call CreateCenteredFloatingWindow()' }
let $FZF_DEFAULT_OPTS="--reverse " " top to bottom

" use rg by default
if executable('rg')
  let $FZF_DEFAULT_COMMAND = 'rg --files --hidden --follow --glob "!.git/*"'
  set grepprg=rg\ --vimgrep
  command! -bang -nargs=* Find call fzf#vim#grep('rg --column --line-number --no-heading --fixed-strings --ignore-case --hidden --follow --glob "!.git/*" --color "always" '.shellescape(<q-args>).'| tr -d "\017"', 1, <bang>0)
endif

" floating fzf window with borders
function! CreateCenteredFloatingWindow()
    let width = min([&columns - 4, max([80, &columns - 20])])
    let height = min([&lines - 4, max([20, &lines - 10])])
    let top = ((&lines - height) / 2) - 1
    let left = (&columns - width) / 2
    let opts = {'relative': 'editor', 'row': top, 'col': left, 'width': width, 'height': height, 'style': 'minimal'}

    let top = "╭" . repeat("─", width - 2) . "╮"
    let mid = "│" . repeat(" ", width - 2) . "│"
    let bot = "╰" . repeat("─", width - 2) . "╯"
    let lines = [top] + repeat([mid], height - 2) + [bot]
    let s:buf = nvim_create_buf(v:false, v:true)
    call nvim_buf_set_lines(s:buf, 0, -1, v:true, lines)
    call nvim_open_win(s:buf, v:true, opts)
    set winhl=Normal:Floating
    let opts.row += 1
    let opts.height -= 2
    let opts.col += 2
    let opts.width -= 4
    call nvim_open_win(nvim_create_buf(v:false, v:true), v:true, opts)
    au BufWipeout <buffer> exe 'bw '.s:buf
endfunction
"
" Files + devicons + floating fzf
function! Fzf_dev()
  let l:fzf_files_options = '--preview "bat --theme="OneHalfDark" --style=numbers,changes --color always {2..-1} | head -'.&lines.'"'
  function! s:files()
    let l:files = split(system($FZF_DEFAULT_COMMAND), '\n')
    return s:prepend_icon(l:files)
  endfunction

  function! s:prepend_icon(candidates)
    let l:result = []
    for l:candidate in a:candidates
      let l:filename = fnamemodify(l:candidate, ':p:t')
      let l:icon = WebDevIconsGetFileTypeSymbol(l:filename, isdirectory(l:filename))
      call add(l:result, printf('%s %s', l:icon, l:candidate))
    endfor

    return l:result
  endfunction

  function! s:edit_file(item)
    let l:pos = stridx(a:item, ' ')
    let l:file_path = a:item[pos+1:-1]
    execute 'silent e' l:file_path
  endfunction

  call fzf#run({
        \ 'source': <sid>files(),
        \ 'sink':   function('s:edit_file'),
        \ 'options': '-m --reverse ' . l:fzf_files_options,
        \ 'down':    '40%',
        \ 'window': 'call CreateCenteredFloatingWindow()'})

endfunction


