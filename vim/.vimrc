" vim:foldmethod=marker:foldlevel=0

" first time setup
" 1. :!git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
" 2. :PluginInstall

" custom shortcuts
let mapleader = " "
nnoremap <leader><space> :nohlsearch<CR>
nnoremap <leader>g :Gstatus<CR>
nnoremap <leader>x :Explore<CR>

" plugins start
set rtp+=~/.vim/bundle/Vundle.vim"
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

" fancy bars
Plugin 'vim-airline/vim-airline'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'

" theme (see below)
Plugin 'gruvbox-community/gruvbox'

" code commenting utility
Plugin 'tpope/vim-commentary'

" whitespace management
Plugin 'ntpeters/vim-better-whitespace'
let g:better_whitespace_filetypes_blacklist = ['diff']
let g:better_whitespace_enabled = 1
let g:strip_whitelines_at_eof = 1
let g:strip_whitespace_confirm = 0
let g:strip_whitespace_on_save = 1
let g:strip_max_file_size = 0

" git integration
Plugin 'tpope/vim-fugitive'

" git changes in sign column
Plugin 'airblade/vim-gitgutter'
set updatetime=100
let g:gitgutter_max_signs = -1
let g:gitgutter_sign_added = '++'
let g:gitgutter_sign_modified = '>>'
let g:gitgutter_sign_removed = 'vv'
let g:gitgutter_sign_removed_first_line = '^^'
let g:gitgutter_sign_removed_above_and_below = '||'
let g:gitgutter_sign_modified_removed = 'v>'

" markdown editing
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
let g:vim_markdown_folding_disabled = 1

" plugins end
call vundle#end()

" appearance
syntax on
set hlsearch
set incsearch
set number
set relativenumber
set cursorline
autocmd WinEnter * set cursorline
autocmd WinLeave * set nocursorline

" set theme and fixes
let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_invert_signs = 1
colorscheme gruvbox
set fillchars+=vert:│
highlight SpellBad cterm=NONE ctermbg='red'
highlight SpellCap cterm=NONE ctermbg='blue'

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
set wildmenu
set ignorecase
set smartcase
set ttimeoutlen=0
set notimeout

" remember position
autocmd BufReadPost * silent! normal! `"

" swap files (https://vi.stackexchange.com/a/179)
set directory^=$HOME/.vim//

" persistent undo
set undofile
set undodir=$HOME/.vim/

" netrw
let g:netrw_banner = 0
let g:netrw_alto = 1
let g:netrw_altv = 1

" zoom
set winwidth=80
set winheight=24
autocmd BufWinEnter,WinEnter * :wincmd =
