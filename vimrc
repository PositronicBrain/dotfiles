" Configure backspace so it acts as it should act
set backspace=eol,start,indent
" Enable syntax highlighting
syntax enable
" Use spaces instead of tabs
set expandtab
" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4
set showmode
" set sh=/usr/local/bin/bash

set nocompatible
set modeline

set autoindent
set hlsearch
set incsearch
set list listchars=tab:»·,trail:·
set nobackup
set nowrap
set smartcase

" Enable loading the plugin files for specific file types.
filetype plugin on

" Enable loading the indent file for specific file types.
filetype indent on
set title
" set ruler
set textwidth=72
