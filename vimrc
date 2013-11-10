" Configure backspace so it acts as it should act
set backspace=eol,start,indent
" Enable loading the plugin files for specific file types.
filetype plugin on
" Enable syntax highlighting
syntax enable
" Enable loading the indent file for specific file types.
filetype indent on
" Use spaces instead of tabs
set expandtab
" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4
set showmode
" set sh=/usr/local/bin/bash

set nocompatible
set modeline
set number
set wildmenu
set wildmode=list:longest,full
set omnifunc=syntaxcomplete#Complete
set hidden
set autoindent
set list listchars=tab:»·,trail:·
set nobackup
set nowrap
set ignorecase
set smartcase


set title
" set ruler
set textwidth=72
colorscheme 256-jungle
