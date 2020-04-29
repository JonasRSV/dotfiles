set rtp+=~/.vim/bundle/Vundle.vim


call vundle#begin()
" alternatively, pass a path where Vundle should install plugins

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'junegunn/fzf.vim'
Plugin 'junegunn/fzf'
Plugin 'gabrielsimoes/cfparser.vim'

Plugin 'neovim/nvim-lsp'
Plugin 'junegunn/goyo.vim'
Plugin 'ncm2/ncm2-ultisnips'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'ycm-core/YouCompleteMe'
Plugin 'ryanoasis/vim-devicons'
call vundle#end()            " required

lua << EOF
local nvim_lsp = require'nvim_lsp'
nvim_lsp.bashls.setup{}
nvim_lsp.clangd.setup{}
nvim_lsp.dockerls.setup{}
nvim_lsp.texlab.setup{}
nvim_lsp.ghcide.setup{}
EOF

set omnifunc=v:lua.vim.lsp.omnifunc
let g:ycm_key_list_select_completion=[]
let g:ycm_key_list_previous_completion=[]


set completeopt=noinsert,menuone,noselect


let g:UltiSnipsJumpForwardTrigger	= "<C-m>"
let g:UltiSnipsRemoveSelectModeMappings = 0

set backupdir=/home/jonas/.backups
set directory=/home/jonas/.backups


let g:disable_vim_auto_close_plugin = 1
"Global sets
set laststatus=1
set statusline=%f\ %y

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

nnoremap j gj
nnoremap k gk

map <space> <leader>
map s /
map <C-d> :bd<CR>
map ö 7j
map ä 7k
map Y 0y$
map Z zz

" For syncing clipboard on unix systems with xclip
"map <C-y> yy \| :call system("xclip -selection clipboard -in", @0)<CR>
"map <C-p> :r !xclip -selection clipboard -o<CR>


"stty -ixon IS NEEDED FOR C-s binding put in *rc
nnoremap gw :RgSearchWord<CR>
nnoremap <F2> :silent! Make<CR>

highlight QuickFixLine term=bold,underline cterm=bold,underline gui=bold,underline
highlight Folded guibg=#3a3c3f guifg=#c0c4ce

nnoremap <C-f> :call Fzf_dev()<CR>
nnoremap <C-g> :Rg<CR>

nnoremap F gg=G<C-o><C-o>zz
nnoremap <C-b> :Buffers<CR>
nnoremap <C-space> @:


"LSP keymaps
nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>


command! Make execute "make " . expand("%") . " | redraw!"
command! RgSearchWord execute "Rg " . expand("<cword>") 


au FileType netrw setl bufhidden=delete
au FileType python set equalprg=yapf
au FileType go set equalprg=gofmt
au FileType javascript set equalprg=standard\ --stdin\ --fix


func RenderTex()
  let file="main.tex"
  call system("pdflatex " .  file)
endfun

au FileType tex au BufWritePost <buffer> call RenderTex()

func TexPreview()
  call system("gv -widgetless -spartan -watch " . expand("%:r") . ".pdf")
endfun
command TexPreview silent call TexPreview()

augroup remember_folds
  autocmd!
  autocmd BufWinLeave *.py mkview
  autocmd BufWinLeave *.go mkview
  autocmd BufWinEnter *.py silent! loadview
  autocmd BufWinEnter *.go silent! loadview
augroup END

" Floating Window Stuff

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
