" editing
set tabstop=4
set shiftwidth=4
set expandtab

" appearance
syntax on
colorscheme elflord
set hlsearch
set incsearch

" markdown
let g:markdown_fenced_languages = ['c', 'go', 'java', 'javascript', 'python']

" vundle (git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim)
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

Plugin 'bling/vim-airline'

Plugin 'tpope/vim-commentary'

Plugin 'ntpeters/vim-better-whitespace'
let g:better_whitespace_filetypes_blacklist = []
let g:better_whitespace_enabled = 1
let g:strip_whitelines_at_eof = 1
let g:strip_whitespace_confirm = 0
let g:strip_whitespace_on_save = 1

Plugin 'tpope/vim-fugitive'

call vundle#end()
