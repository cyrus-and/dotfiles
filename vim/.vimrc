" appearance
syntax on
set hlsearch
set incsearch

" theme
colorscheme elflord
highlight VertSplit cterm=NONE ctermfg=darkgray
highlight StatusLine ctermfg=darkgray
highlight StatusLineNC ctermfg=darkgray
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
set modeline

" netrw
let g:netrw_banner = 0

" git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

Plugin 'bling/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
let g:airline#extensions#tabline#enabled = 1

Plugin 'tpope/vim-commentary'

Plugin 'ntpeters/vim-better-whitespace'
let g:better_whitespace_filetypes_blacklist = []
let g:better_whitespace_enabled = 1
let g:strip_whitelines_at_eof = 1
let g:strip_whitespace_confirm = 0
let g:strip_whitespace_on_save = 1

Plugin 'tpope/vim-fugitive'

Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
let g:vim_markdown_folding_disabled = 1

call vundle#end()
