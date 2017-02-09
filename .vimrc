" enable 256 colors in GNOME terminal (for my Ubuntu VM)
if $COLORTERM == 'gnome-terminal'
    set t_Co=256
endif

" set your color scheme (replace wombat with whatever yours is called)
" if you're using a gvim or macvim, then your color scheme may have a version
" that uses more than 256 colors
if has("gui_running")
    colorscheme desert
elseif &t_Co == 256
    colorscheme desert
endif

" python3 from powerline.vim import setup as powerline_setup
" python3 powerline_setup()
" python3 del powerline_setup

" Be smart when using tabs ;)
set smarttab

filetype plugin indent on
" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab
set number

call pathogen#infect()
call pathogen#helptags()

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'tpope/vim-fugitive'
" plugin from http://vim-scripts.org/vim/scripts.html
Plugin 'L9'
" Git plugin not hosted on GitHub
Plugin 'git://git.wincent.com/command-t.git'
" git repos on your local machine (i.e. when working on your own plugin)
Plugin 'file:///home/gmarik/path/to/plugin'
" The sparkup vim script is in a subdirectory of this repo called vim.
" Pass the path to set the runtimepath properly.
Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}

" Solarized Color Scheme
Plugin 'altercation/vim-colors-solarized'
" Scala syntax coloring
Plugin 'derekwyatt/vim-scala'

" All of your Plugins must be added before the following line
call vundle#end()            " required

syntax on
set clipboard=unnamedplus
