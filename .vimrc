set rtp+=~/.vim/bundle/Vundle.vim


call vundle#begin()
" alternatively, pass a path where Vundle should install plugins

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Utilities
Plugin 'kkoomen/vim-doge' " Auto doc comment
Plugin 'scrooloose/nerdcommenter' "Auto comment
Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'

" Neovim LSP
Plugin 'hrsh7th/nvim-compe'
Plugin 'neovim/nvim-lspconfig'
Plugin 'nvim-lua/popup.nvim'
Plugin 'nvim-lua/plenary.nvim'
Plugin 'nvim-telescope/telescope.nvim'
Plugin 'ray-x/lsp_signature.nvim'
Plugin 'hrsh7th/vim-vsnip'
Plugin 'hrsh7th/vim-vsnip-integ'
Plugin 'windwp/nvim-autopairs'
Plugin 'rafamadriz/friendly-snippets'
Plugin 'onsails/lspkind-nvim'
Plugin 'simrat39/rust-tools.nvim'


" Code forces
Plugin 'gabrielsimoes/cfparser.vim'

Plugin 'cespare/vim-toml'

" FZF + LSP
Plugin 'ojroques/nvim-lspfuzzy'

" Niceities
Plugin 'ryanoasis/vim-devicons'

" Navigation
Plugin 'majutsushi/tagbar'
Plugin 'ms-jpq/chadtree'

" Language Specific
Plugin 'rust-lang/rust.vim' 
Plugin 'evanleck/vim-svelte'
Plugin 'uarun/vim-protobuf'
Plugin 'cakebaker/scss-syntax.vim'
Plugin 'chr4/nginx.vim'

" Git 
Plugin 'tpope/vim-fugitive'
call vundle#end()            " required


let g:chadtree_settings = { "theme.text_colour_set": "solarized_universal" }
let g:python3_host_prog = '/usr/bin/python3.8'
set completeopt=menuone,noselect


lua << EOF

-- vim.lsp.set_log_level("debug")
require('nvim-autopairs').setup()


local util = require 'lspconfig/util'



require('telescope').setup{
  defaults = {
    vimgrep_arguments = {
      'rg',
      '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--smart-case'
    },
    prompt_prefix = "> ",
    selection_caret = "> ",
    entry_prefix = "  ",
    initial_mode = "insert",
    selection_strategy = "reset",
    sorting_strategy = "descending",
    layout_strategy = "horizontal",
    layout_config = {
      horizontal = {
        mirror = false,
      },
      vertical = {
        mirror = false,
      },
    },
    file_sorter =  require'telescope.sorters'.get_fuzzy_file,
    file_ignore_patterns = {},
    generic_sorter =  require'telescope.sorters'.get_generic_fuzzy_sorter,
    winblend = 0,
    border = {},
    borderchars = { '─', '│', '─', '│', '╭', '╮', '╯', '╰' },
    color_devicons = true,
    use_less = true,
    path_display = {},
    set_env = { ['COLORTERM'] = 'truecolor' }, -- default = nil,
    file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
    grep_previewer = require'telescope.previewers'.vim_buffer_vimgrep.new,
    qflist_previewer = require'telescope.previewers'.vim_buffer_qflist.new,

    -- Developer configurations: Not meant for general override
    buffer_previewer_maker = require'telescope.previewers'.buffer_previewer_maker
  }
}


require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  resolve_timeout = 800;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = {
    border = { '', '' ,'', ' ', '', '', '', ' ' }, -- the border option is the same as `|help nvim_open_win|`
    winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
    max_width = 120,
    min_width = 60,
    max_height = math.floor(vim.o.lines * 0.3),
    min_height = 1,
  };

  source = {
    path = true;
    buffer = true;
    calc = true;
    nvim_lsp = true;
    nvim_lua = true;
    vsnip = true;
    ultisnips = true;
    luasnip = true;
  };
}

require("nvim-autopairs.completion.compe").setup({
  map_cr = true, --  map <CR> on insert mode
  map_complete = true -- it will auto insert `(` after select function or method item
})

require "lsp_signature".setup()
require('lspfuzzy').setup {}

require('lspkind').init({
    -- enables text annotations
    --
    -- default: true
    with_text = true,

    -- default symbol map
    -- can be either 'default' or
    -- 'codicons' for codicon preset (requires vscode-codicons font installed)
    --
    -- default: 'default'
    preset = 'codicons',

    -- override preset symbols
    --
    -- default: {}
    symbol_map = {
      Text = "",
      Method = "",
      Function = "",
      Constructor = "",
      Field = "ﰠ",
      Variable = "",
      Class = "ﴯ",
      Interface = "",
      Module = "",
      Property = "ﰠ",
      Unit = "塞",
      Value = "",
      Enum = "",
      Keyword = "",
      Snippet = "",
      Color = "",
      File = "",
      Reference = "",
      Folder = "",
      EnumMember = "",
      Constant = "",
      Struct = "פּ",
      Event = "",
      Operator = "",
      TypeParameter = ""
    },
})

local lspconfig = require('lspconfig')
local configs = require'lspconfig/configs'

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

lspconfig.rust_analyzer.setup{
  capabilities = capabilities,
}
lspconfig.clangd.setup{
  capabilities = capabilities,
}
lspconfig.svelte.setup{
  capabilities = capabilities,
}
lspconfig.bashls.setup{
  capabilities = capabilities,
}
lspconfig.pyright.setup{
  capabilities = capabilities,
}

if not lspconfig.emmet_ls then    
  configs.emmet_ls = {    
    default_config = {    
      cmd = {'emmet-ls', '--stdio'};
      filetypes = {'html', 'css'};
      root_dir = function(fname)    
        return vim.loop.cwd()
      end;    
      settings = {};    
    };    
  }    
end

lspconfig.emmet_ls.setup{ 
  capabilities = capabilities,
}

lspconfig.html.setup {
  capabilities = capabilities,
  cmd = { "vscode-html-language-server", "--stdio" },
  filetypes = { "html" },
  init_options = {
    configurationSection = { "html", "css", "javascript" },
    embeddedLanguages = {
      css = true,
      javascript = true
    }
  },
  root_dir = util.root_pattern(".git"),
  settings = {}
}

lspconfig.cssls.setup {
  capabilities = capabilities,
  cmd = { "vscode-css-language-server", "--stdio" },
    filetypes = { "css", "scss", "less" },
    root_dir = util.root_pattern(".git"),
    settings = {
      css = {
        validate = true
      },
      less = {
        validate = true
      },
      scss = {
        validate = true
      }
    }
}

local opts = {
    tools = { -- rust-tools options
        -- automatically set inlay hints (type hints)
        -- There is an issue due to which the hints are not applied on the first
        -- opened file. For now, write to the file to trigger a reapplication of
        -- the hints or just run :RustSetInlayHints.
        -- default: true
        autoSetHints = true,

        -- whether to show hover actions inside the hover window
        -- this overrides the default hover handler so something like lspsaga.nvim's hover would be overriden by this
        -- default: true
        hover_with_actions = true,

        runnables = {
            -- whether to use telescope for selection menu or not
            -- default: true
            use_telescope = true

            -- rest of the opts are forwarded to telescope
        },

        debuggables = {
            -- whether to use telescope for selection menu or not
            -- default: true
            use_telescope = true

            -- rest of the opts are forwarded to telescope
        },

        -- These apply to the default RustSetInlayHints command
        inlay_hints = {
            -- wheter to show parameter hints with the inlay hints or not
            -- default: true
            show_parameter_hints = true,

            -- prefix for parameter hints
            -- default: "<-"
            parameter_hints_prefix = "<- ",

            -- prefix for all the other hints (type, chaining)
            -- default: "=>"
            other_hints_prefix = "=> ",

            -- whether to align to the length of the longest line in the file
            max_len_align = false,

            -- padding from the left if max_len_align is true
            max_len_align_padding = 1,

            -- whether to align to the extreme right or not
            right_align = false,

            -- padding from the right if right_align is true
            right_align_padding = 7
        },

        hover_actions = {
            -- the border that is used for the hover window
            -- see vim.api.nvim_open_win()
            border = {
                {"╭", "FloatBorder"}, {"─", "FloatBorder"},
                {"╮", "FloatBorder"}, {"│", "FloatBorder"},
                {"╯", "FloatBorder"}, {"─", "FloatBorder"},
                {"╰", "FloatBorder"}, {"│", "FloatBorder"}
            },

            -- whether the hover action window gets automatically focused
            -- default: false
            auto_focus = false
        }
    },

    -- all the opts to send to nvim-lspconfig
    -- these override the defaults set by rust-tools.nvim
    -- see https://github.com/neovim/nvim-lspconfig/blob/master/CONFIG.md#rust_analyzer
    server = {} -- rust-analyer options
}

require('rust-tools').setup(opts)


EOF

let g:loaded_fzf_vim = 1


"inoremap <silent><expr> <CR>  compe#confirm('<CR>')

nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> K  <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>

nnoremap <C-t> :Telescope<CR>


imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'


set backupdir=/home/jonas/.backups
set directory=/home/jonas/.backups


let g:disable_vim_auto_close_plugin = 1
"Global sets
set laststatus=0
set splitbelow

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

if has("nvim")
  nnoremap <C-j> :bp<CR>`"zz
  nnoremap <C-k> :bn<CR>`"zz
  nnoremap <leader><C-space> :vertical sbp<CR>`"zz
else
  nnoremap <C-j> :bp<CR>`"zz
  nnoremap <C-j> :bn<CR>`"zz
  map <leader><C-@> :vertical sbp<CR>`"zz
endif


inoremap (<CR> ()<Esc>i
inoremap {<CR> {<CR>}<Esc>O
inoremap {; {<CR>};<Esc>O
inoremap {, {<CR>},<Esc>O
inoremap [<CR> []<Esc>i
inoremap [; [];<Esc>i<Esc>i
inoremap [, [],<Esc>i<Esc>i

nnoremap <leader>p <C-^>`"zz
nnoremap n nzz
nnoremap N Nzz

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


highlight StatusLine guibg=#00000 guifg=#b8ff73
highlight QuickFixLine term=bold,underline cterm=bold,underline gui=bold,underline
highlight Folded guibg=#3a3c3f guifg=#c0c4ce
hi FoldColumn guifg=#00000 guibg=#00000
hi SignColumn guifg=#00000 guibg=#00000


nnoremap tb :TagbarToggle<CR>


nnoremap F gg=G<C-o><C-o>zz
"nnoremap <C-j> :cnext<CR>
"nnoremap <C-k> :cprev<CR>


command! Make execute "make " . expand("%") . " | redraw! | vertical cope | vertical resize 100 | wincmd p"


au BufRead,BufNewFile *.journal set filetype=journal

au FileType netrw setl bufhidden=delete
au FileType go setlocal equalprg=gofmt
au FileType javascript setlocal equalprg=js-beautify\ --stdin
au FileType haskell setlocal equalprg=stylish-haskell
au FileType json setlocal equalprg=js-beautify

au FileType python setlocal makeprg=python3\ %
au FileType python setlocal equalprg=yapf
au FileType python compiler python
au FileType python nnoremap <F10> :silent exec "!python3 %"<CR>

au FileType html nnoremap <buffer> <leader><F10> :!xdg-open %<CR>
au FileType html inoremap <silent><expr> <C-Space> compe#complete()

au FileType cpp inoremap <silent><expr> <C-Space> compe#complete()
au FileType cpp setlocal equalprg=clang-format

au FileType rust nnoremap <buffer> <C-space> :RustHoverActions<CR>
au FileType rust nnoremap <leader>t :RustTest<CR>



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

