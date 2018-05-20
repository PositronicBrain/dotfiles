call plug#begin('~/.vim/plugged')
" Gist support
Plug 'mattn/webapi-vim'
Plug 'mattn/gist-vim'

" Smooth scrolling
Plug 'yuttie/comfortable-motion.vim'

" Colorscheme and statusline
" Plug 'nanotech/jellybeans.vim'
" Plug 'vim-scripts/Solarized'
" Plug 'morhetz/gruvbox'
" Plug 'sickill/vim-monokai'
" Plug 'rainux/vim-desert-warm-256'
" Plug 'w0ng/vim-hybrid'
" Plug 'chriskempson/base16-vim'
" Plug 'bluz71/vim-moonfly-colors'
" Plug 'bluz71/vim-moonfly-statusline'
Plug 'andreasvc/vim-256noir'

" Go to root of the project when opening a file
Plug 'airblade/vim-rooter'

" Better startup screen
Plug 'mhinz/vim-startify'

" Improvements to the vim file explorer
Plug 'jeetsukumaran/vim-buffergator'

" Git integration
Plug 'tpope/vim-fugitive'

" Color hex codes
" This breaks vim
" Plug 'chrisbra/Colorizer'
"
" Better table management
Plug 'dhruvasagar/vim-table-mode'

" :FixWhiteSpace
Plug 'bronson/vim-trailing-whitespace'

" Comment portion of text
Plug 'scrooloose/nerdcommenter'

" Linting
Plug 'w0rp/ale'

" Search into files with :Ack
Plug 'mileszs/ack.vim'

" :Delete :Move :Rename :Chmod: Mkdir :Find :Locate: :Wall :SudoWrite :SudoEdit
Plug 'tpope/vim-eunuch'

" Dispatch commands asynchronoulsy
Plug 'tpope/vim-dispatch'

" Unimpaired, ]q [q
Plug 'tpope/vim-unimpaired'

" Javascript plugins
Plug 'pangloss/vim-javascript', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'mxw/vim-jsx'
" html plugins
Plug 'mattn/emmet-vim/'

" Solidity development
Plug 'tomlion/vim-solidity'

" Latex support
Plug 'vim-latex/vim-latex'

"Purescript
Plug 'frigoeu/psc-ide-vim'
Plug 'raichoo/purescript-vim'

" Graphql syntax
Plug 'jparise/vim-graphql'

" Rust
Plug 'rust-lang/rust.vim'
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'prabirshrestha/asyncomplete-emoji.vim'
Plug 'prabirshrestha/asyncomplete-file.vim'
Plug 'prabirshrestha/asyncomplete-buffer.vim'

" glsl
Plug 'tikhomirov/vim-glsl'
call plug#end()

set nocompatible
filetype off
let mapleader = ","
set visualbell           " don't beep
set noerrorbells         " don't beep
filetype plugin on
filetype indent on
set backspace=eol,start,indent
syntax on
set noshowmode
set title titlestring=vim
set title titlestring+=\ 
set title titlestring+=%F
set encoding=utf-8
setglobal fileencoding=utf-8
" text search options
set smartcase ignorecase incsearch hlsearch

" Remove toolbars
set guioptions-=M  "remove menu bar
set guioptions-=T  "remove toolbar

set t_Co=256
set background=dark
let base16colorspace=256

" colorscheme desert-warm-256
" colorscheme moonfly

" BEGIN COLORSCHEME
colorscheme 256_noir
" Change highlighting of cursor line when entering/leaving Insert Mode
set cursorline
highlight CursorLine cterm=NONE ctermfg=NONE ctermbg=233 guifg=NONE guibg=#121212
autocmd InsertEnter * highlight CursorLine cterm=NONE ctermfg=NONE ctermbg=234 guifg=NONE guibg=#1c1c1c
autocmd InsertLeave * highlight CursorLine cterm=NONE ctermfg=NONE ctermbg=233 guifg=NONE guibg=#121212
" END COLORSCHEME
" BEGIN STATUSLINE
set laststatus=2 " always show
" Show linter warning and errors with ale
function! LinterStatus() abort
   let l:counts = ale#statusline#Count(bufnr(''))
   let l:all_errors = l:counts.error + l:counts.style_error
   let l:all_non_errors = l:counts.total - l:all_errors
   return l:counts.total == 0 ? '' : printf(
   \ 'W:%d E:%d',
   \ l:all_non_errors,
   \ l:all_errors
   \)
endfunction

let g:currentmode={
    \ 'n'  : 'Normal',
    \ 'no' : 'Normal·Operator Pending',
    \ 'v'  : 'Visual',
    \ 'V'  : 'V·Line',
    \ '^V' : 'V·Block',
    \ 's'  : 'Select',
    \ 'S'  : 'S·Line',
    \ '^S' : 'S·Block',
    \ 'i'  : 'Insert',
    \ 'R'  : 'Replace',
    \ 'Rv' : 'V·Replace',
    \ 'c'  : 'Command',
    \ 'cv' : 'Vim Ex',
    \ 'ce' : 'Ex',
    \ 'r'  : 'Prompt',
    \ 'rm' : 'More',
    \ 'r?' : 'Confirm',
    \ '!'  : 'Shell',
    \ 't'  : 'Terminal'
    \}

set noshowmode
set statusline=%{fugitive#statusline()}
" set statusline+=%0*\ %n\                                 " Buffer number
" set statusline+=%1*\ %<%F%m%r%h%w\                       " File path, modified, readonly, helpfile, preview
" set statusline+=%3*│                                     " Separator
set statusline+=%2*\ %Y\                                 " FileType
set statusline+=%3*│                                     " Separator
set statusline+=%2*\ %{''.(&fenc!=''?&fenc:&enc).''}     " Encoding
set statusline+=\ (%{&ff})                               " FileFormat (dos/unix..)
set statusline+=%=                                       " Right Side
set statusline+=%2*\ col:\ %02v\                         " Colomn number
set statusline+=%3*│                                     " Separator
set statusline+=%1*\ ln:\ %02l/%L\ (%3p%%)\              " Line number / total lines, percentage of document
set statusline+=%0*%{toupper(g:currentmode[mode()])}\  " The current mode
set statusline+=%3*\ %{LinterStatus()}
" hi User1 ctermfg=007 ctermbg=239 guibg=#4e4e4e guifg=#adadad
" hi User2 ctermfg=007 ctermbg=236 guibg=#303030 guifg=#adadad
" hi User3 ctermfg=236 ctermbg=236 guibg=#303030 guifg=#303030
" hi User4 ctermfg=239 ctermbg=239 guibg=#4e4e4e guifg=#4e4e4e
" END STATUSLINE

set modeline
set number

" Wildmenu, minibuffer completion
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
set guifont=monaco\ 18
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

" BEGIN TEX
let g:tex_flavor='latex'
" Use unicode for math symbols
" set conceallevel=0
" Disable folding
let Tex_FoldedSections=""
let Tex_FoldedEnvironments=""
let Tex_FoldedMisc=""
" END TEX

" set omnifunc=syntaxcomplete#Complete

" Nerd table
let g:table_mode_separator = '|'
let g:table_mode_corner = '+'
let g:table_mode_corner_corner = '+'
let g:table_mode_fillchar = '-'

" display ambiguous unicode characters on two columns
set ambiwidth=double

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
let g:colorizer_auto_filetype='css,html,js,vim'

let g:ale_linters = { 'javascript': ['standard'] }
let g:ale_fixers = { 'javascript': ['standard']  }

set shortmess+=c   " Shut off completion messages
set belloff+=ctrlg " If Vim beeps during completion
" No other configuration is needed. Just start pressing <tab> or <s-tab>
" to complete a word. If you want to enable automatic completion at startup, put

let g:mucomplete#enable_auto_at_startup = 1

" don't change to any directory when non project files
let g:rooter_change_directory_for_non_project_files = ''
" show directory tree
let g:netrw_liststyle = 3
" open files in the same window
let g:netrw_browse_split = 0

let g:startify_session_dir = '~/.vim/session'

" Use jsx extension also in .js files
let g:jsx_ext_required = 0

" let g:racer_cmd = "/Users/federico/.cargo/bin/racer"
" let g:racer_experimental_completer = 1
" mac
" for rust rls
if executable('rls')
   au User lsp_setup call lsp#register_server({
        \ 'name': 'rls',
        \ 'cmd': {server_info->['rustup', 'run', 'nightly', 'rls']},
        \ 'whitelist': ['rust'],
        \ })
endif
" async complete emoji
au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#emoji#get_source_options({
    \ 'name': 'emoji',
    \ 'whitelist': ['*'],
    \ 'blacklist': ['rust'],
    \ 'completor': function('asyncomplete#sources#emoji#completor'),
    \ }))
" async complete file
au User asyncomplete_setup call asyncomplete#register_source(asyncomplete#sources#file#get_source_options({
    \ 'name': 'file',
    \ 'whitelist': ['*'],
    \ 'priority': 10,
    \ 'completor': function('asyncomplete#sources#file#completor')
    \ }))
let g:rust_clip_command = 'pbcopy'

" bindings for async complete
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"

nnoremap <silent> K :call lsp#ui#vim#hover#get_hover_under_cursor() <CR>
nnoremap <silent> gd :call lsp#ui#vim#definition() <CR>
nnoremap <silent> <F2> :call lsp#ui#vim#rename() <CR>
let g:asyncomplete_remove_duplicates = 1

" gist-vim config
" let g:gist_clip_command = 'pbcopy'
let g:gist_detect_filetype = 1
let g:gist_open_browser_after_post = 1
let g:gist_post_private = 1 " make gists private by default

" set completeopt+=preview
" autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif
" Nerdcommenter
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1

" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'

" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1

" Enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1
