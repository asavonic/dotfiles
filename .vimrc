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
" Plugin 'scrooloose/nerdtree' 	    	" Project and file navigation
Plugin 'majutsushi/tagbar'          	" Class/module browser
Plugin 'szw/vim-ctrlspace' 
Plugin 'Valloric/YouCompleteMe'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'jlanzarotta/bufexplorer'

"------------------=== Other ===----------------------
Plugin 'bling/vim-airline'   	    	" Lean & mean status/tabline for vim
Plugin 'fisadev/FixedTaskList.vim'  	" Pending tasks list
Plugin 'rosenfeld/conque-term'      	" Consoles as buffers
Plugin 'tpope/vim-surround'	   	" Parentheses, brackets, quotes, XML tags, and more
Plugin 'tpope/vim-commentary'	" Enchanced comments
Plugin 'tpope/vim-abolish'	    " nice plugins to test fixing
Plugin 'tpope/vim-fugitive'	    " amazing plugin for git integration
Plugin 'tpope/vim-unimpaired'   " Quickfix bindings
Plugin 'jiangmiao/auto-pairs'   " auto pairs for brackets
Plugin 'altercation/vim-colors-solarized' " themes
Plugin 'octol/vim-cpp-enhanced-highlight'
Plugin 'scrooloose/syntastic'

"--------------=== Snippets support ===---------------
Plugin 'garbas/vim-snipmate'		" Snippets manager
Plugin 'MarcWeber/vim-addon-mw-utils'	" dependencies #1
Plugin 'tomtom/tlib_vim'		" dependencies #2
Plugin 'honza/vim-snippets'		" snippets repo

"---------------=== Languages support ===-------------
" --- Python ---
"Plugin 'klen/python-mode'	        " Python mode (docs, refactor, lints, highlighting, run and ipdb and more)
"Plugin 'davidhalter/jedi-vim' 		" Jedi-vim autocomplete plugin
"Plugin 'mitsuhiko/vim-jinja'		" Jinja support for vim
"Plugin 'mitsuhiko/vim-python-combined'  " Combined Python 2/3 for Vim

call vundle#end()            		" required

filetype on
filetype plugin on
filetype plugin indent on

"=====================================================
" General settings
"=====================================================


set backspace=indent,eol,start
aunmenu Help.
aunmenu Window.
let no_buffers_menu=1
set mousemodel=popup

set ruler
set completeopt-=preview
set gcr=a:blinkon0
if has("gui_running")
  set cursorline
endif
set ttyfast

" включить подсветку кода
syntax on
if has("gui_running")
" GUI? устаналиваем тему и размер окна
set guioptions-=m  "remove menu bar
set guioptions-=M  "remove menu bar
set guioptions-=T  "remove toolbar
set guioptions-=r  "remove right-hand scroll bar
set guioptions-=L  "remove left-hand scroll bar
  set lines=50 columns=125
  colorscheme molokai
" раскомментируйте эти строки, если хотите, чтобы NERDTree/TagBar автоматически отображались при запуске vim
" autocmd vimenter * TagbarToggle
" autocmd vimenter * NERDTree
" autocmd vimenter * if !argc() | NERDTree | endif

" на маке vim?
if has("mac")
  set guifont=Consolas:h13
  set fuoptions=maxvert,maxhorz
else
" дефолтный GUI
  set guifont=Ubuntu\ Mono\ derivative\ Powerline\ 10
endif
else
" терминал?
"  colorscheme myterm
endif

" colorscheme
set t_Co=256
set background=dark
colorscheme asavonic


" tab sball
" set switchbuf=useopen

" отключаем пищалку и мигание
set visualbell t_vb= 
set novisualbell       

set enc=utf-8	     " utf-8 по дефолту в файлах
set ls=2             " всегда показываем статусбар
set incsearch	     " инкреминтируемый поиск
set hlsearch	     " подсветка результатов поиска
set nu	             " показывать номера строк
set scrolloff=5	     " 5 строк при скролле за раз

" прячем панельки
"set guioptions-=m   " меню
set guioptions-=T    " тулбар
"set guioptions-=r   "  скроллбары

" настройка на spaces
set smartindent
set tabstop=4
set shiftwidth=4
set expandtab

" указываем каталог с настройками SnipMate
let g:snippets_dir = "~/.vim/vim-snippets/snippets"

" настройки Vim-Airline
set laststatus=2
let g:airline_theme='badwolf'
let g:airline_powerline_fonts = 0

"tabline conflicts with ctrl_space
"
"let g:airline#extensions#tabline#enabled = 1
"let g:airline#extensions#tabline#formatter = 'unique_tail'
"let g:airline#extensions#tabline#show_buffers = 0
"
" for ControlSpace
let g:ctrlspace_use_tabline = 1
let g:airline_exclude_preview = 1 
set autochdir
autocmd BufEnter, BufNewFile * silent! lcd %:p:h

" TagBar настройки
map <F4> :TagbarToggle<CR>
let g:tagbar_autofocus = 0 " автофокус на Tagbar при открытии

" NerdTree настройки
" показать NERDTree на F3
" map <F3> :NERDTreeToggle<CR>
"игноррируемые файлы с расширениями
" let NERDTreeIgnore=['\~$', '\.pyc$', '\.pyo$', '\.class$', 'pip-log\.txt$', '\.o$']  

" TaskList настройки
map <F2> :TaskList<CR> 	   " отобразить список тасков на F2

" Работа буфферами
map <C-q> :bd<CR> 	   " CTRL+Q - закрыть текущий буффер

" ranger integration
function! Ranger()
    " Get a temp file name without creating it
    let tmpfile = substitute(system('mktemp -u'), '\n', '', '')
    let cwd = getcwd()
    silent exec '!ranger --choosefile='.tmpfile.' '.cwd
    " If the temp file has been written by ranger
    if filereadable(tmpfile)
        " Get the selected file name from the temp file
        let filetoedit = system('cat '.tmpfile)
        call delete(tmpfile)
        exec 'edit '.filetoedit
    endif
    redraw!
endfunction
 
nmap <F3> :call Ranger()<cr>

" Swap and backup
"set backupdir=/tmp/asavonic/backup//
"set directory=/tmp/asavonic/swap//
"set undodir=/tmp/asavonic/undo//

let g:formatprg_args_cpp = "--style=stroustrup --indent=spaces=4 --max-instatement-indent=50 --break-blocks --pad-oper --pad-paren --align-pointer=type --add-brackets --mode=c"
let g:formatprg_args_cl  = g:formatprg_args_cpp
let g:formatprg_args_h   = g:formatprg_args_cpp
let g:formatprg_args_hpp = g:formatprg_args_cpp
let g:formatprg_args_c   = g:formatprg_args_cpp
let g:formatprg_cpp = "astyle"
let g:formatprg_cl  = "astyle"
let g:formatprg_h   = "astyle"
let g:formatprg_hpp = "astyle"
let g:formatprg_c   = "astyle"
noremap <F6> :Autoformatall<CR><CR>

" YouCompleteMe config
let g:ycm_seed_identifiers_with_syntax = 1 " extract tags from vim syntax
let g:ycm_collect_identifiers_from_tags_files = 1 " grep from tags
let g:ycm_extra_conf_globlist = ['/localdisk/git/*']

:nnoremap <C-l> :YcmCompleter GoTo<CR>
" BufExplorer 
let g:bufExplorerSortBy='mru'
nn gb : :BufExplorer<CR>

:nnoremap <C-n> :bnext<CR>
:nnoremap <C-p> :bprevious<CR>

" redefine of leader key
let mapleader = ","

" disable warnings with swapfiles
set shortmess+=A


" prevent cursor to move, when esc from insert mode
inoremap <silent> <Esc> <C-O>:stopinsert<CR>
