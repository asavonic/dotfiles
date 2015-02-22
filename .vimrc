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

Plugin 'kien/ctrlp.vim'                     " fuzzy search
Plugin 'mileszs/ack.vim'

"---------=== Code complete ===-------------
Plugin 'Valloric/YouCompleteMe'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'

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


"---------=== CtrlP ===-------------
let g:ctrlp_use_caching = 0
if executable('ag')
    set grepprg=ag\ --nogroup\ --nocolor

    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
else
  let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']
  let g:ctrlp_prompt_mappings = {
    \ 'AcceptSelection("e")': ['<space>', '<cr>', '<2-LeftMouse>'],
    \ }
endif

"---------=== Ack/Ag support ===-------------
if executable('ag')
    let g:ackprg = 'ag --vimgrep'
endif

"---------=== UltiSnips ===-------------
let g:UltiSnipsSnippetDirectories=["UltiSnips"]


"=====================================================
" Keybindings
"=====================================================

map <F4> :TagbarToggle<CR>
imap jk <Esc>          " fast ESC replacement


"---------=== Leader mappings ===-------------
" Space mapping is awesome
let mapleader = "\<Space>"
" save current file
nnoremap <Leader>w :w<CR>
" open file using CtrlP
nnoremap <Leader>o :CtrlP<CR>    
" CTRL+Q - close current buffer
map <Leader>q :bd<CR> 	   

" copy/paste from system clipboard
vmap <Leader>y "+y
vmap <Leader>d "+d
nmap <Leader>p "+p
nmap <Leader>P "+P
vmap <Leader>p "+p
vmap <Leader>P "+P

"---------=== GoTo Declaration/Definition ===-------------
autocmd Filetype c,cpp noremap <Leader>d :YcmCompleter GoToDeclaration<CR>
autocmd Filetype c,cpp noremap <Leader>r :YcmCompleter GoToDefinition<CR>

"---------=== YouCompleteMe + UltiSnips ===-------------
autocmd Filetype c,cpp noremap <Leader>d :YcmCompleter GoToDeclaration<CR>
" make YCM compatible with UltiSnips
let g:ycm_key_list_select_completion=['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion=['<C-p>', '<Up>']

let g:UltiSnipsExpandTrigger="<Tab>"
let g:UltiSnipsJumpForwardTrigger="<Tab>"                                           
let g:UltiSnipsJumpBackwardTrigger="<S-Tab>"
