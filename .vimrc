set nocompatible              " be iMproved, required
filetype off                  " required

"=====================================================
" Vundle settings
"=====================================================
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'		" let Vundle manage Vundle, required

"---------=== Code/project navigation ===-------------
Plugin 'majutsushi/tagbar'          	" Class/module browser
Plugin 'szw/vim-ctrlspace'              " Project manager
Plugin 'Lokaltog/vim-easymotion'        " improves movement
Plugin 'jlanzarotta/bufexplorer'        " popup window with opened buffers

"---------=== Code complete ===-------------
Plugin 'Valloric/YouCompleteMe'

"---------=== Look and feel ===-------------
Plugin 'itchyny/lightline.vim'          " minimal powerline-airline replacement
Plugin 'morhetz/gruvbox'                " favorite colorscheme



call vundle#end()            		" required

filetype on
filetype plugin on
filetype plugin indent on

"=====================================================
" General settings
"=====================================================

set backspace=indent,eol,start
set timeoutlen=1000 ttimeoutlen=0 " fixes freeze when exiting from insert mode

set nu	                  " show line numbers

set visualbell t_vb=      " disable visual bell
set novisualbell       

set enc=utf-8	     
set ls=2                  " always show statusbar
set incsearch	          " enable incremental search
set hlsearch	          " search highlighting
set scrolloff=5	          " start scrolling on nth line from top/bottom


"---------=== Spaces ===-------------
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab


"---------=== GUI mode ===-------------

if has("gui_running")
    set cursorline
    aunmenu *
    set guioptions-=m   " no menu
    set guioptions-=T   " no toolbar
    set guioptions-=r   " no scrollbar
endif

"---------=== Colorscheme ===-------------
set t_Co=256
set background=dark
let g:gruvbox_italic=0
colorscheme gruvbox

"=====================================================
" Plugins
"=====================================================

"---------=== TagBar ===-------------
let g:tagbar_autofocus = 1

"---------=== YouCompleteMe ===-------------
let g:ycm_seed_identifiers_with_syntax = 1  " collect identifiers from vim-syntax files
let g:ycm_global_ycm_extra_conf = '~/.vim/config/ycm_global_conf.py'

"=====================================================
" Keybindings
"=====================================================

map <F4> :TagbarToggle<CR>
map <C-q> :bd<CR> 	   " CTRL+Q - close current buffer
imap jk <Esc>          " fast ESC replacement
