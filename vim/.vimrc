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

" EasyMotion {{{
Plugin 'easymotion/vim-easymotion'
let g:EasyMotion_do_mapping = 0
" }}}

" Fugitive {{{
Plugin 'tpope/vim-fugitive'
Plugin 'aymericbeaumet/vim-symlink'
" }}}

" fzf {{{
Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'
let g:fzf_layout = { 'down': '100%' }
command! -bang -nargs=* Rg call fzf#vim#grep("rg --column --line-number --no-heading --color=always --smart-case --hidden ".shellescape(<q-args>), 1, {'options': '--delimiter : --nth 4..'}, <bang>0) " XXX this nonsense here is to avoid :Rg grep the filename...
" }}}

" IndentLine {{{
Plugin 'Yggdroot/indentLine'
autocmd BufEnter NERD_tree* :IndentLinesDisable
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

" Gruvbox {{{
Plugin 'gruvbox-community/gruvbox'
let g:gruvbox_contrast_dark = 'hard'
let g:gruvbox_guisp_fallback = 'bg'
let g:gruvbox_invert_signs = 1
" }}}

" Highlightedyank {{{
Plugin 'machakann/vim-highlightedyank'
" }}}

" JavaScript {{{
Plugin 'pangloss/vim-javascript'
Plugin 'MaxMEllon/vim-jsx-pretty'
" }}}

" liquid {{{
Plugin 'tpope/vim-liquid'
" }}}

" Markdown {{{
Plugin 'godlygeek/tabular' " XXX must be before
Plugin 'plasticboy/vim-markdown'
let g:vim_markdown_conceal = 0
let g:vim_markdown_conceal_code_blocks = 0
let g:vim_markdown_folding_disabled = 1
" }}}

" NERDTree {{{
Plugin 'preservim/nerdtree'
let g:NERDTreeChDirMode = 2
let g:NERDTreeQuitOnOpen = 1
let g:NERDTreeDirArrowCollapsible = '-'
let g:NERDTreeDirArrowExpandable = '+'
let g:NERDTreeShowHidden=1
" }}}

" Rooter {{{
Plugin 'airblade/vim-rooter'
let g:rooter_silent_chdir = 1
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
autocmd BufReadPost * silent! normal! g`"
filetype plugin indent on
set directory^=$HOME/.vim// " swap files directory (https://vi.stackexchange.com/a/179)
set hidden
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
highlight link mkdLineBreak ExtraWhitespace " XXX https://github.com/plasticboy/vim-markdown/issues/289
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
set backup
set backupdir=$HOME/.vim/
" }}}

" GUI {{{
set background=dark
set guicursor+=a:blinkon0
set guifont=Iosevka\ SS04:h15
set guioptions-=l
set guioptions-=L
set guioptions-=r
set guioptions-=R
set belloff=all
set fullscreen
" also set:
" defaults write org.vim.MacVim MMNativeFullScreen 0
" defaults write org.vim.MacVim MMTextInsetTop 0
" defaults write org.vim.MacVim MMTextInsetLeft 0
" defaults write org.vim.MacVim MMTextInsetBottom 0
" defaults write org.vim.MacVim MMTextInsetRight 0
" }}}

" Highlight annoying characters {{{
autocmd BufWinEnter * match ErrorMsg /[ ‘’“”]/
" }}}

" JSON {{{
let g:vim_json_conceal = 0
" }}}

" Netrw {{{
let g:netrw_alto = 1
let g:netrw_altv = 1
let g:netrw_keepdir= 0
" }}}

" Shortcuts {{{
let mapleader = " "
nnoremap <leader>n :nohlsearch<CR>
nnoremap <leader>s :Git<CR><C-W>T
nnoremap <leader>S :GFiles?<CR>
nnoremap <leader>l :Commits<CR>
nnoremap <leader>f :GitFiles<CR>
nnoremap <leader>t :NERDTree<CR>
nnoremap <leader>. :NERDTreeFind<CR>
nnoremap <leader>F :Files<CR>
nnoremap <leader>g :Rg<CR>
nnoremap <leader>b :Buffer<CR>
nnoremap <leader>h :History<CR>
nnoremap <leader>z :wincmd =<CR>
nmap <leader>j <Plug>(easymotion-overwin-w)
" }}}

" Zoom {{{
" autocmd BufWinEnter,WinEnter * :wincmd =
" set winheight=30
" set winwidth=120
" }}}
