set nocompatible
" set nofoldenable    " disable folding
filetype off

" Avoid timeoutes when building modules
let g:neobundle#install_process_timeout = 1500
call neobundle#begin(expand('~/.vim/neobundle/'))
NeoBundle 'Shougo/vimproc.vim', {
            \ 'build' : {
            \     'windows' : 'tools\\update-dll-mingw',
            \     'cygwin' : 'make -f make_cygwin.mak',
            \     'mac' : 'make -f make_mac.mak',
            \     'linux' : 'make',
            \     'unix' : 'gmake',
            \    },
            \ }
" Automatic completion for different languages, including javascript
" cd .vim/neobundle/YouCompleteMe
" bash install.sh
NeoBundle 'Valloric/YouCompleteMe', {
            \ 'build'      : {
            \ 'mac'     : './install.py',
            \ 'unix'    : './install.py',
            \ 'windows' : 'install.py',
            \ 'cygwin'  : './install.py'
            \ }
            \ }
" Haskell plugins
NeoBundle 'Shougo/neocomplete.vim'
NeoBundle 'eagletmt/neco-ghc'            " requires neocomplete.vim
NeoBundle 'eagletmt/ghcmod-vim'          " requires vimproc.vim
NeoBundle 'dag/vim2hs'
NeoBundle 'travitch/hasksyn'
" General editing plugins
NeoBundle 'dhruvasagar/vim-table-mode'
NeoBundle 'bronson/vim-trailing-whitespace' " :FixWhiteSpace
NeoBundle 'scrooloose/nerdcommenter'
" Color themes
NeoBundle 'nanotech/jellybeans.vim'
NeoBundle 'vim-scripts/Solarized'
NeoBundle 'morhetz/gruvbox'
NeoBundle 'sickill/vim-monokai'
NeoBundle 'rainux/vim-desert-warm-256'
NeoBundle 'w0ng/vim-hybrid'
" Javascript plugins
NeoBundle 'jelera/vim-javascript-syntax' , {'autoload':{'filetypes':['javascript']}}
NeoBundle 'pangloss/vim-javascript'
" cd .vim/neobundle/tern_for_vim
" npm install
NeoBundle 'ternjs/tern_for_vim' " improves on youcompleteme for javascript
" html plugins
NeoBundle 'mattn/emmet-vim/'
" Linting
NeoBundle 'scrooloose/syntastic'
"Auto matching parentheses
NeoBundle 'Raimondi/delimitMate'
" Better status line
NeoBundle 'bling/vim-airline'
call neobundle#end()

filetype plugin indent on
set backspace=eol,start,indent
syntax on
set showmode
set title
set smartcase ignorecase incsearch hlsearch
set laststatus=2
set modeline
set number
set wildmenu
set wildmode=list:longest,full
set wildignore=*.o,*.obj,*~         "stuff to ignore when tab completing
set wildignore+=*DS_Store*
set wildignore+=*.png,*.jpg,*.gif,*.pdf,*.psd
set omnifunc=syntaxcomplete#Complete
set hidden
set nrformats-=octal
set listchars=tab:‣\ ,trail:␣
set list
" The following two options turn off physical line wrappings
set textwidth=0
set wrapmargin=0
set nowrap " no visual wrapping
set formatoptions=cq
set scrolloff=4
set hidden
set autoindent expandtab tabstop=4 softtabstop=4 shiftwidth=4
" Highlight columns longer than 80
augroup vimrc_autocmds
autocmd BufEnter * highlight OverLength ctermbg=1  guibg=#592929
autocmd BufEnter * match OverLength  /\%>80v.\+/
augroup END

set autowrite
set showmatch
"set statusline=%<%F\ %h%m%r%{fugitive#statusline()}%=%-14.(%l/%L,%c%V%)\ %y
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
let g:acp_enableAtStartup = 1 "Enable automatic popup
let g:neocomplete#enable_at_startup = 1
" necoghc
setlocal omnifunc=necoghc#omnifunc
let g:necoghc_enable_detailed_browse = 1
"ghcmod-vim
autocmd BufWritePost *.hs,*.lhs GhcModCheckAndLintAsync

let g:table_mode_separator = '|'
let g:table_mode_corner = '+'
let g:table_mode_corner_corner = '+'
let g:table_mode_fillchar = '-'

set t_Co=256
set background=dark
colorscheme gruvbox

set guioptions-=M  "remove menu bar
set guioptions-=T  "remove toolbar
set guifont=DejaVu\ Sans\ Mono\ 15
" ntrw config
let g:netrw_browse_split = 4 "  vertically splitting the window first  
let g:netrw_altv = 1
let g:netrw_liststyle=3
let g:netrw_winsize = 25

" Youcomplete me config, no new winds
let g:ycm_add_preview_to_completeopt=0
let g:ycm_confirm_extra_conf=0
set completeopt-=preview
" js linter: eslint
let g:syntastic_javascript_checkers = ['eslint']
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
