" appearance
syntax on
set hlsearch
set incsearch

" theme
colorscheme elflord
highlight VertSplit cterm=NONE ctermfg=234
highlight StatusLine ctermfg=234
highlight StatusLineNC ctermfg=234
set fillchars+=vert:│

" editing
set tabstop=4
set shiftwidth=4
set expandtab
autocmd Filetype markdown setlocal linebreak

" spell checking
autocmd Filetype gitcommit,markdown setlocal spell

" behavior
filetype plugin indent on
set modelines=5 " revert to vim default option

" netrw
let g:netrw_banner = 0
let g:netrw_alto = 1
let g:netrw_altv = 1

" git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

Plugin 'bling/vim-airline'
let g:airline#extensions#tabline#enabled = 1

Plugin 'tpope/vim-commentary'

Plugin 'ntpeters/vim-better-whitespace'
let g:better_whitespace_filetypes_blacklist = []
let g:better_whitespace_enabled = 1
let g:strip_whitelines_at_eof = 1
let g:strip_whitespace_confirm = 0
let g:strip_whitespace_on_save = 1
let g:strip_max_file_size = 0

Plugin 'tpope/vim-fugitive'

Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
let g:vim_markdown_folding_disabled = 1

call vundle#end()
