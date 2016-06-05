set nocompatible
" set nofoldenable    " disable folding
filetype off


call plug#begin('~/.vim/plugged')
Plug 'airblade/vim-rooter'
" Better startup screen
Plug 'mhinz/vim-startify'
Plug 'ctrlpvim/ctrlp.vim'
" Improvements to the vim file explorer
Plug 'tpope/vim-vinegar'

Plug 'Shougo/vimproc.vim', { 'do': 'make' }
" Git integration
Plug 'tpope/vim-fugitive'

" Plug 'Valloric/YouCompleteMe', { 'do': './install.py --tern-completer' }

" Programming languages autocompletion
Plug 'Shougo/neocomplete.vim'

" General editing plugins
Plug 'dhruvasagar/vim-table-mode'
" :FixWhiteSpace
Plug 'bronson/vim-trailing-whitespace'
Plug 'scrooloose/nerdcommenter'
" Color themes
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

 " improves on youcompleteme for javascript
Plug 'ternjs/tern_for_vim' , { 'do': 'npm install' }
" html plugins
Plug 'mattn/emmet-vim/'
" Linting
Plug 'scrooloose/syntastic'
" Prefer local eslint version
Plug 'pmsorhaindo/syntastic-local-eslint.vim'
"Auto matching parentheses
Plug 'Raimondi/delimitMate'
" Better status line
" Plug 'bling/vim-airline'

Plug 'mileszs/ack.vim'
" Plug 'ryanoasis/vim-devicons'
" Plug 'itchyny/lightline.vim'

Plug 'tpope/vim-eunuch'
call plug#end()
" show buffers in line
" let g:airline#extensions#tabline#enabled = 1

set statusline=                                     " Override default
set statusline+=%1*%{fugitive#statusline()[4:-2]}%* " Show fugitive git info
set statusline+=%2*\ %f\ %m\ %r%*                   " Show filename/path
set statusline+=%3*%=%*                             " Set right-side status info after this line
set statusline+=%4*%l/%L:%v%*                       " Set <line number>/<total lines>:<column>
set statusline+=%5*\ %*                             " Set ending space
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
set guifont=Monaco:h17
set nowrap " no visual wrapping
set formatoptions=cq
set scrolloff=4
set autoindent
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4
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
" neocomplete
" Disable AutoComplPop.
" let g:acp_enableAtStartup = 1 "Enable automatic popup
let g:neocomplete#enable_at_startup = 1
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

colorscheme base16-tomorrow
" display ambiguous unicode characters on two columns
set ambiwidth=double
set guioptions-=M  "remove menu bar
set guioptions-=T  "remove toolbar

" Syntastic
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_loc_list_height = 5
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1

let g:syntastic_error_symbol = '!'
" let g:syntastic_style_error_symbol = '⁉️'
let g:syntastic_warning_symbol = '?'
" let g:syntastic_style_warning_symbol = '💩'

" highlight link SyntasticErrorSign SignColumn
" highlight link SyntasticWarningSign SignColumn
" highlight link SyntasticStyleErrorSign SignColumn
" highlight link SyntasticStyleWarningSign SignColumn
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

" icons in airline
" let g:airline_powerline_fonts = 1
