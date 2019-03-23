" Of course
set nocompatible

execute pathogen#infect()
syntax on
filetype plugin indent on

" Window handling
nnoremap <C-W>m <C-W>h|xnoremap <C-W>m <C-W>h|
nnoremap <C-W>n <C-W>j|xnoremap <C-W>n <C-W>j|
nnoremap <C-W>e <C-W>k|xnoremap <C-W>e <C-W>k|
nnoremap <C-W>i <C-W>l|xnoremap <C-W>i <C-W>l|
nnoremap + 10<C-W><|xnoremap <C-W>< 10<C-W><|
nnoremap - 10<C-W>>|xnoremap <C-W>> 10<C-W>>|

set termguicolors
colorscheme dracula
set path+=**

set number            "Number lines
set title

set tabstop=2         "A tab is 2 spaces
set softtabstop=2     "Insert 2 spaces when tab is pressed
set shiftwidth=2      "An indent is 2 spaces

set expandtab	        "Always uses spaces instead of tabs
set shiftround        "Round indent to nearestshiftwidth multiple

set shell=/run/current-system/sw/bin/fish
set ignorecase
set incsearch
set nohlsearch

set colorcolumn=100
set nowrap

set foldmethod=indent
set foldlevel=99
nnoremap <space> za

set splitright
set splitbelow

let g:airline_theme='dracula'
let g:loaded_python_provider=1
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_python_flake8_args = '--ignore="E501,E201, W503"'

nnoremap ZX ZQ

let g:SimpylFold_docstring_preview=1

" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

let g:ultisnips_python_style = "google"
let g:snips_author = "my name"
let g:snips_email = "example@gmail.com"
let g:snips_github = "claymager"
