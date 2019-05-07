set nocompatible
set encoding=utf8

" Source the vimrc file after saving it
autocmd! bufwritepost ~/.vimrc source $MYVIMRC

" turn on syntax highlighting, with slate colorscheme
syntax enable
colorscheme slate
highlight MatchParen ctermbg=darkgray
filetype plugin indent on

" Search dictionary too
set dict=/usr/share/dict/words
set complete+=k

" Persistent undo
if !isdirectory($HOME . "/.vim/undo")
  silent call mkdir ($HOME . '/.vim/undo', 'p', 0700)
endif
set undodir=~/.vim/undo
set undofile

set wrap
set number
set relativenumber
set ruler
set showmode
set tw=80
set lazyredraw
set foldmethod=syntax
set foldlevelstart=1
" Don't wrap lines when editing
set formatoptions-=t
if v:version > 703
  " remove comment leader when joining comment lines
  set formatoptions+=j
endif

" turn off bells
set noerrorbells
set vb t_vb=

if &term =~ '256color'
  " disable Background Color Erase (BCE)
  " see http://snk.tuxfamily.org/log/vim-256color-bce.html
  set t_ut=
endif

set smarttab
set smartindent
set autoindent
set softtabstop=2
set tabstop=2
set shiftwidth=2
set expandtab

" change cursor to a vertical line when in editing mode
set t_Co=256
let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_EI = "\<Esc>]50;CursorShape=0\x7"
hi! CursorLine ctermbg=000
hi! CursorColumn ctermbg=000

" search options
set smartcase
set hlsearch
set incsearch
hi Search ctermfg=white ctermbg=173 cterm=none guifg=#ffffff guibg=#e5786d gui=none
" hi! link Visual Search
" tab to escape and turn off highlighting
nnoremap <Tab> :noh<CR><C-g>

set mouse=
set history=1000
set clipboard=unnamedplus,autoselect
set completeopt+=menuone,menu,longest
set cmdheight=2

" search into subfolders and with wildcards for better tab-completion
set path+=**
set wildignore+=*/tmp/*,*.swp,*.swo,*.zip,.git,.cabal-sandbox
set wildmode=list:longest,full
set wildmenu

" disable ex mode
nnoremap Q <nop>

" space to save
nnoremap <Space> :w<CR>
" return to run q macro
nnoremap <CR> @q

" number of lines to keep below cursor when scolling
set so=15

" show trailing whitespace
set list
if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

" add/remove a comment in various file formats
au BufNewFile,BufRead,BufEnter .vimrc map <silent> - @='I" <C-V><Esc>0j'<CR>
au BufNewFile,BufRead,BufEnter .vimrc map <silent> _ :s/^\( *\)" \?/\1/e<Enter>:noh<Enter>0j
au BufNewFile,BufRead,BufEnter *.js map <silent> - @='I// <C-V><Esc>0j'<CR>
au BufNewFile,BufRead,BufEnter *.js map <silent> _ :s/^\( *\)\/\/ \?/\1/e<Enter>:noh<Enter>0j
au BufNewFile,BufRead,BufEnter *.py map <silent> - @='I# <C-V><Esc>0j'<CR>
au BufNewFile,BufRead,BufEnter *.py map <silent> _ :s/^\( *\)# \?/\1/e<Enter>:noh<Enter>0j
au BufNewFile,BufRead,BufEnter *.hs map <silent> - @='I-- <C-V><Esc>0j'<CR>
au BufNewFile,BufRead,BufEnter *.hs map <silent> _ :s/^\( *\)-- \?/\1/e<Enter>:noh<Enter>0j
au BufNewFile,BufRead,BufEnter *.html map <silent> - @='I<!-- <C-V><Esc>A --><C-V><Esc>0j'<CR>
au BufNewFile,BufRead,BufEnter *.html map <silent> _ /--><CR>?<!--<CR>:s/^\( *\)\%(\(<\)!-- \?\(\/\?[^<]\+>\)\\|<!-- \?\)/\1\2\3/e<CR>/--><CR>:s/\(<\/\?[a-zA-Z]\+\) \?--\(>\)\\| \?-->/\1\2/e<CR>:noh<CR>0j
au BufNewFile,BufRead,BufEnter *.html vmap <buffer> - <C-C>`>a --<C-V>><Esc>`<i<!-- <Esc>

" formatting for haskell
au BufNewFile,BufRead,BufEnter *.hs setlocal formatprg=hindent

" https://github.com/mpickering/hlint-refactor-vim
function! ApplyOneSuggestion()
  let l = line(".")
  let c = col(".")
  let l:filter = "%! hlint - --refactor --refactor-options=\"--pos ".l.','.c."\""
  execute l:filter
  silent if v:shell_error == 1 | undo | endif
  call cursor(l, c)
endfunction

function! ApplyAllSuggestions()
  let l = line(".")
  let c = col(".")
  let l:filter = "%! hlint - --refactor"
  execute l:filter
  silent if v:shell_error == 1 | undo | endif
  call cursor(l, c)
endfunction

au BufNewFile,BufRead,BufEnter *.hs map <silent> to :call ApplyOneSuggestion()<CR>
au BufNewFile,BufRead,BufEnter *.hs map <silent> ta :call ApplyAllSuggestions()<CR>
