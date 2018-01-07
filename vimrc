set nocompatible
" set nofoldenable    " disable folding
filetype off


call plug#begin('~/.vim/plugged')
" Smooth scrolling
Plug 'yuttie/comfortable-motion.vim'

" Colorscheme and statusline
Plug 'bluz71/vim-moonfly-colors'
Plug 'bluz71/vim-moonfly-statusline'

" Go to root of the project when opening a file
Plug 'airblade/vim-rooter'

" Better startup screen
Plug 'mhinz/vim-startify'

" Improvements to the vim file explorer
Plug 'jeetsukumaran/vim-buffergator'

" Git integration
Plug 'tpope/vim-fugitive'

" Plug 'Shougo/vimproc.vim', { 'do': 'make' }
" Programming languages autocompletion
" Plug 'Shougo/neocomplete.vim'
" Plug 'maralla/completor.vim'
" Plug 'Valloric/YouCompleteMe', { 'do': './install.py --tern-completer' }

Plug 'lifepillar/vim-mucomplete'


" Color hex codes
Plug 'chrisbra/Colorizer'
"
" Better table management
Plug 'dhruvasagar/vim-table-mode'

" :FixWhiteSpace
Plug 'bronson/vim-trailing-whitespace'

" Comment portion of text
Plug 'scrooloose/nerdcommenter'

" Various Color themes
Plug 'nanotech/jellybeans.vim'
Plug 'vim-scripts/Solarized'
Plug 'morhetz/gruvbox'
Plug 'sickill/vim-monokai'
Plug 'rainux/vim-desert-warm-256'
Plug 'w0ng/vim-hybrid'
Plug 'chriskempson/base16-vim'

" Javascript plugins
" Plug 'jelera/vim-javascript-syntax', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'mxw/vim-jsx'
 " improves on youcompleteme for javascript
" Plug 'ternjs/tern_for_vim' , { 'do': 'npm install' }
" html plugins
Plug 'mattn/emmet-vim/'
" Linting
Plug 'w0rp/ale'
" Plug 'scrooloose/syntastic'
" Prefer local eslint version
" Plug 'mtscout6/syntastic-local-eslint.vim'

" Search into files with :Ack
Plug 'mileszs/ack.vim'
" Plug 'ryanoasis/vim-devicons'

" :Delete :Move :Rename :Chmod: Mkdir :Find :Locate: :Wall :SudoWrite :SudoEdit

Plug 'tpope/vim-eunuch'

" Solidity development
Plug 'tomlion/vim-solidity'

"Purescript
Plug 'frigoeu/psc-ide-vim'
Plug 'raichoo/purescript-vim'

" Graphql syntax
Plug 'jparise/vim-graphql'

" Rust
Plug 'rust-lang/rust.vim'

call plug#end()

let mapleader = ","
set visualbell           " don't beep
set noerrorbells         " don't beep

filetype plugin on
filetype indent on
set backspace=eol,start,indent
syntax on
set noshowmode
set title
" text search options
set smartcase ignorecase incsearch hlsearch
set laststatus=2
set modeline
set number
" Wildmenu
set wildmenu
set wildmode=list:longest,full
set wildignore=*.o,*.obj,*~         "stuff to ignore when tab completing
set wildignore+=*DS_Store*
set wildignore+=*.png,*.jpg,*.gif,*.pdf,*.psd

set hidden
set nrformats-=octal
set listchars=tab:‣\ ,trail:␣
set list
" The following two options turn off physical line wrappings
set textwidth=0
set wrapmargin=0
" set guifont=DejaVu\ Sans\ Mono\ 15
set guifont=Monaco:h18
set nowrap " no visual wrapping
set formatoptions=cq
set scrolloff=4
set autoindent
set tabstop=2
set softtabstop=2
set expandtab
set shiftwidth=2
" Highlight columns longer than 80
augroup vimrc_autocmds
autocmd BufEnter * highlight OverLength ctermbg=1  guibg=#592929
autocmd BufEnter * match OverLength  /\%>80v.\+/
augroup END

set autowrite
set showmatch
set noswapfile
set equalalways eadirection=both

" Sane behaviour when pasting from X clipboard
nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set showmode

autocmd BufRead,BufNewFile *.txt,*.tex set fo=cqt tw=72 wm=0

let g:tex_flavor='latex'
" Use unicode for math symbols
" set conceallevel=0
set encoding=utf-8
setglobal fileencoding=utf-8
" Disable folding
let Tex_FoldedSections=""
let Tex_FoldedEnvironments=""
let Tex_FoldedMisc=""
"
" Uncomment the following to enable neocomplete for every supported filetype
set omnifunc=syntaxcomplete#Complete

" Nerd table
let g:table_mode_separator = '|'
let g:table_mode_corner = '+'
let g:table_mode_corner_corner = '+'
let g:table_mode_fillchar = '-'

set t_Co=256
set background=dark
let base16colorspace=256

" colorscheme desert-warm-256
colorscheme moonfly
" display ambiguous unicode characters on two columns
set ambiwidth=double

" Remove toolbars
set guioptions-=M  "remove menu bar
set guioptions-=T  "remove toolbar

" Turn off search highlight
nnoremap <leader><space> :nohlsearch<CR>

" Sane backup settings
set backup
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set backupskip=/tmp/*
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set writebackup

" Useful to speed up macros
"set lazyredraw

" Split horizontally and sort by filepath
let g:buffergator_viewport_split_policy = 'T'
let g:buffergator_sort_regime = 'filepath'
let g:buffergator_display_regime = 'filepath'

" Colorize hex codes in the following files
let g:colorizer_auto_filetype='css,html,js'

let g:ale_linters = { 'javascript': ['standard'] }
let g:ale_fixers = { 'javascript': ['standard']  }


" mucomplete setup
set completeopt+=menuone
set completeopt+=noselect
inoremap <expr> <c-e> mucomplete#popup_exit("\<c-e>")
inoremap <expr> <c-y> mucomplete#popup_exit("\<c-y>")
inoremap <expr>  <cr> mucomplete#popup_exit("\<cr>")

set shortmess+=c   " Shut off completion messages
set belloff+=ctrlg " If Vim beeps during completion
" No other configuration is needed. Just start pressing <tab> or <s-tab> to complete a word. If you want to enable automatic completion at startup, put

let g:mucomplete#enable_auto_at_startup = 1

" don't change to any directory when non project files
let g:rooter_change_directory_for_non_project_files = ''
" show directory tree
let g:netrw_liststyle = 3
" open files in the same window
let g:netrw_browse_split = 0

"show argument hints
let g:tern_show_argument_hints='on_hold'
let g:startify_session_dir = '~/.vim/session'

" Use jsx extension also in .js files
let g:jsx_ext_required = 0
