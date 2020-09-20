" vim:foldmethod=marker:nofoldenable

" First-time setup {{{
" :!git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
" :PluginInstall
" }}}

" Plugins {{{

" Vundle (begin) {{{
set rtp+=~/.vim/bundle/Vundle.vim"
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" }}}

" Airline {{{
Plugin 'vim-airline/vim-airline'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#tabline#show_close_button = 0
let g:airline#extensions#tabline#show_splits = 0
let g:airline#extensions#tabline#show_tab_count = 0
let g:airline#extensions#tabline#show_tab_nr = 0
let g:airline#extensions#tabline#show_tab_type = 0
" }}}

" Better Whitespace {{{
Plugin 'ntpeters/vim-better-whitespace'
autocmd FileType fugitive DisableWhitespace
let g:better_whitespace_enabled = 1
let g:better_whitespace_filetypes_blacklist = ['diff']
let g:strip_max_file_size = 0
let g:strip_whitelines_at_eof = 1
let g:strip_whitespace_confirm = 0
let g:strip_whitespace_on_save = 1
" }}}

" Commentary {{{
Plugin 'tpope/vim-commentary'
" }}}

" CtrlP {{{
Plugin 'ctrlpvim/ctrlp.vim'
let g:ctrlp_custom_ignore = 'node_modules'
let g:ctrlp_map = '<leader>p'
let g:ctrlp_match_current_file = 1
let g:ctrlp_regexp = 1
let g:ctrlp_show_hidden = 1
" }}}

" Fugitive {{{
Plugin 'tpope/vim-fugitive'
Plugin 'aymericbeaumet/vim-symlink'
" }}}

" IndentLine {{{
Plugin 'Yggdroot/indentLine'
let g:indentLine_char = '┆'
let g:indentLine_concealcursor = 0
" }}}

" Gitgutter {{{
Plugin 'airblade/vim-gitgutter'
set foldtext=gitgutter#fold#foldtext()
set updatetime=100
let g:gitgutter_max_signs = -1
let g:gitgutter_sign_added = '++'
let g:gitgutter_sign_modified = '>>'
let g:gitgutter_sign_removed = 'vv'
let g:gitgutter_sign_removed_first_line = '^^'
let g:gitgutter_sign_removed_above_and_below = '||'
let g:gitgutter_sign_modified_removed = 'v>'
" }}}

" GnuPG {{{
Plugin 'jamessan/vim-gnupg'
" }}}

" Gruvbox {{{
Plugin 'gruvbox-community/gruvbox'
let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_guisp_fallback = 'bg'
let g:gruvbox_invert_signs = 1
" }}}

" Goyo {{{
Plugin 'junegunn/goyo.vim'

function! s:goyo_enter()
    mkexrc! $HOME/.vim/mkexrc
    set display+=lastline
    set fullscreen
    set nocursorline
    noremap k gk
    noremap j gj
    noremap <Up> gk
    noremap <Down> gj
    noremap <D-Left> g0
    noremap <D-Right> g$
endfunction

function! s:goyo_leave()
    source $HOME/.vim/mkexrc
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()
" }}}

" Markdown {{{
Plugin 'godlygeek/tabular' " XXX must be before
Plugin 'plasticboy/vim-markdown'
let g:vim_markdown_conceal = 0
let g:vim_markdown_conceal_code_blocks = 0
let g:vim_markdown_folding_disabled = 1
" }}}

" Vundle (end) {{{
call vundle#end()
" }}}

" }}}

" Appearance {{{
syntax on
set fillchars+=vert:│
set hlsearch
set incsearch
set number
set relativenumber
" }}}

" Behavior {{{
autocmd BufReadPost * silent! normal! `"
filetype plugin indent on
set directory^=$HOME/.vim// " swap files directory (https://vi.stackexchange.com/a/179)
set ignorecase
set modelines=5 " revert to vim default option on macOS
set shortmess+=I
set smartcase
set ttimeoutlen=0
set wildmenu
set wildmode=list:longest,full
" }}}

" Colorscheme {{{
colorscheme gruvbox
highligh link mkdLineBreak ExtraWhitespace " XXX https://github.com/plasticboy/vim-markdown/issues/289
" }}}

" Cursor line {{{
set cursorline
autocmd WinEnter * set cursorline
autocmd WinLeave * set nocursorline
" }}}

" Editing {{{
autocmd FileType markdown setlocal linebreak
autocmd FileType markdown,gitcommit setlocal spell
set backspace=indent,eol,start
set clipboard=unnamed
set expandtab
set linebreak
set shiftwidth=4
set tabstop=4
set undofile
set undodir=$HOME/.vim/
" }}}

" GUI {{{
set background=dark
set guicursor+=a:blinkon0
set guifont=Iosevka\ SS04:h17
set guioptions-=l
set guioptions-=L
set guioptions-=r
set guioptions-=R
set belloff=all
" }}}

" Highlight annoying characters {{{
autocmd BufWinEnter * match ErrorMsg /[ ‘’“”]/
" }}}

" Netrw {{{
let g:netrw_alto = 1
let g:netrw_altv = 1
let g:netrw_banner = 0
let g:netrw_keepdir= 0
" }}}

" Shortcuts {{{
let mapleader = " "
nnoremap <leader><space> :nohlsearch<CR>
nnoremap <leader>s :Gstatus<CR>
nnoremap <leader>t :Texplore<CR>
nnoremap <leader>x :Rexplore<CR>
" }}}

" Zoom {{{
autocmd BufWinEnter,WinEnter * :wincmd =
set winheight=30
set winwidth=120
" }}}
