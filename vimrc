set nocompatible
set nofoldenable    " disable folding
filetype off

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
NeoBundle 'Shougo/neocomplete.vim'
NeoBundle 'eagletmt/neco-ghc'            " requires neocomplete.vim
NeoBundle 'eagletmt/ghcmod-vim'          " requires vimproc.vim
NeoBundle 'dag/vim2hs'
NeoBundle 'travitch/hasksyn'
NeoBundle 'dhruvasagar/vim-table-mode'
NeoBundle 'bronson/vim-trailing-whitespace' " :FixWhiteSpace
NeoBundle 'scrooloose/nerdcommenter'
" Color themes
NeoBundle 'nanotech/jellybeans.vim'
NeoBundle 'vim-scripts/Solarized'
NeoBundle 'morhetz/gruvbox'
NeoBundle 'sickill/vim-monokai'
NeoBundle 'rainux/vim-desert-warm-256'
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
set omnifunc=syntaxcomplete#Complete
set hidden
set nrformats-=octal
set autoindent smartindent expandtab tabstop=4 softtabstop=4 shiftwidth=4
set listchars=tab:‣\ ,trail:␣
set list
set nobackup
" The following two options turn off physical line wrappings
set textwidth=0
set wrapmargin=0
set nowrap " no visual wrapping
set formatoptions=cq
set scrolloff=4
set hidden
" Highlight columns longer than 80
augroup vimrc_autocmds
autocmd BufEnter * highlight OverLength ctermbg=1  guibg=#592929
autocmd BufEnter * match OverLength  /\%>80v.\+/
augroup END

set autowrite
set showmatch
set statusline=%<%F\ %h%m%r%{fugitive#statusline()}%=%-14.(%l/%L,%c%V%)\ %y
set noswapfile
set equalalways eadirection=both

" Sane behaviour when pasting from other apps
nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set showmode

autocmd BufRead,BufNewFile *.txt,*.tex set fo=cqt tw=72 wm=0

let g:tex_flavor='latex'
" Use unicode for math symbols
set conceallevel=0
set encoding=utf-8
setglobal fileencoding=utf-8
" Disable folding
:let Tex_FoldedSections=""
:let Tex_FoldedEnvironments=""
:let Tex_FoldedMisc=""
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
