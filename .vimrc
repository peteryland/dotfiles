set encoding=utf-8
set fileencoding=utf-8

" source the vimrc file after saving it
autocmd! bufwritepost ~/.vimrc source $MYVIMRC

" turn on syntax highlighting, with slate colorscheme
syntax enable
colorscheme slate
highlight MatchParen ctermbg=darkgray
filetype plugin indent on

" completion options
set dict=/usr/share/dict/words
set complete=.,w,b,u,t,d,i,kspell
set completeopt=menu

function! DoTabPre()
  set complete=.,t,d,i
  set completeopt=longest,menuone
  return ''
endfunction
function! DoTabPost()
  set complete=.,w,b,u,t,d,i,kspell
  set completeopt=menu
  return ''
endfunction
inoremap <expr> <silent> <C-@> pumvisible()? "\<C-p>" : "\<C-p>\<C-n>"
inoremap <expr> <silent> <C-SPACE> pumvisible()? "\<C-p>" : "\<C-p>\<C-n>"
inoremap <silent> <TAB> <C-R>=DoTabPre()<CR><C-n><C-R>=DoTabPost()<CR>
inoremap <silent> <C-x><C-l> <C-R>=DoTabPre()<CR><C-x><C-l><C-R>=DoTabPost()<CR>
inoremap <silent> <C-x><C-f> <C-R>=DoTabPre()<CR><C-x><C-f><C-R>=DoTabPost()<CR>

" persistent undo
if !isdirectory($HOME . "/.vim/undo")
  silent call mkdir ($HOME . '/.vim/undo', 'p', 0700)
endif
set undodir=~/.vim/undo
set undofile

" other options
set wrap
set magic
set number
set relativenumber
set ruler
set showmode
set tw=80
set lazyredraw
set notimeout ttimeout timeoutlen=100
nnoremap j gj
nnoremap gj j
nnoremap k gk
nnoremap gk k

" turn on folding
set foldmethod=syntax
set foldlevelstart=4

" don't wrap lines when editing
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

" tab to turn off search highlighting and show status
nnoremap <Tab> :noh<CR><C-g>

" backspace to quit all, if possible
nnoremap <silent> <BS> :qa<CR>

set mouse=
nnoremap <silent> zma :set mouse=a<cr>
nnoremap <silent> zmo :set mouse=<cr>
set history=1000
set cmdheight=2
set backspace=indent,eol,start
set splitbelow splitright
" set laststatus=2

if system('uname -s') == "Darwin\n"
  set clipboard=unnamed,autoselect "OSX
else
  set clipboard=unnamedplus,autoselect "Linux
endif

" search into subfolders and with wildcards for better tab-completion
set path+=**
set wildignore+=*/tmp/*,*.swp,*.swo,*.zip,.git,.cabal-sandbox
set wildmode=list:longest,full
set wildmenu

" disable ex mode
nnoremap Q <nop>

" space to save, ctrl-space to save and switch to next window
nnoremap <SPACE> :w<CR>
nnoremap <C-@> :w<CR><C-w>w
nnoremap <C-SPACE> :w<CR><C-w>w
tnoremap <F1> <C-w>N
tnoremap <C-@> <C-w>w
tnoremap <C-SPACE> <C-w>w
tnoremap <C-v> <C-w>""

" return to run q macro
nnoremap <CR> @q

" quickfix open, up, down, next file
map <C-h> :cw<CR>
map <C-j> :cn<CR>
map <C-k> :cp<CR>
map <C-l> :cnf<CR>

" turn on spell checker
set spell spelllang=en_au
hi clear SpellBad
hi clear SpellCap
hi clear SpellRare
hi clear SpellLocal
hi SpellBad cterm=underline
hi SpellLocal cterm=underline
hi SpellBad gui=undercurl
hi SpellLocal gui=undercurl

" working with splits
nnoremap <S-UP> :resize +2<CR>
nnoremap <S-DOWN> :resize -2<CR>
nnoremap <S-LEFT> :vertical resize +2<CR>
nnoremap <S-RIGHT> :vertical resize -2<CR>
tnoremap <S-UP> <C-w>:resize +2<CR>
tnoremap <S-DOWN> <C-w>:resize -2<CR>
tnoremap <S-LEFT> <C-w>:vertical resize +2<CR>
tnoremap <S-RIGHT> <C-w>:vertical resize -2<CR>

nnoremap zh <C-W>h
nnoremap zj <C-W>j
nnoremap zk <C-W>k
nnoremap zl <C-W>l
nnoremap zH <C-W>H
nnoremap zJ <C-W>J
nnoremap zK <C-W>K
nnoremap zL <C-W>L

" number of lines to keep below cursor when scrolling
set scrolloff=15

" show trailing whitespace
set list
if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

" add/remove a line comment in various file formats
map <silent> - @='gI# <C-V><Esc>0j'<CR>
map <silent> _ :s/^\( *\)# \?/\1/e<Enter>:noh<Enter>0j
au BufNewFile,BufRead,BufEnter .vimrc,*.vim map <silent> - @='gI" <C-V><Esc>0j'<CR>
au BufNewFile,BufRead,BufEnter .vimrc,*.vim map <silent> _ :s/^\( *\)" \?/\1/e<Enter>:noh<Enter>0j
au BufNewFile,BufRead,BufEnter *.c,*.h,*.cpp,*.java,*.js map <silent> - @='gI// <C-V><Esc>0j'<CR>
au BufNewFile,BufRead,BufEnter *.c,*.h,*.cpp,*.java,*.js map <silent> _ :s/^\( *\)\/\/ \?/\1/e<Enter>:noh<Enter>0j
au BufNewFile,BufRead,BufEnter *.hs,.ghci map <silent> - @='gI-- <C-V><Esc>0j'<CR>
au BufNewFile,BufRead,BufEnter *.hs,.ghci map <silent> _ :s/^\( *\)-- \?/\1/e<Enter>:noh<Enter>0j
au BufNewFile,BufRead,BufEnter *.lhs map <silent> - :s/^> /> -- /e<Enter>:noh<Enter>0j
au BufNewFile,BufRead,BufEnter *.lhs map <silent> _ :s/^> -- /> /e<Enter>:noh<Enter>0j
au BufNewFile,BufRead,BufEnter *.html,*.st map <silent> - @='gI<!-- <C-V><Esc>A --><C-V><Esc>0j'<CR>
au BufNewFile,BufRead,BufEnter *.html,*.st map <silent> _ /--><CR>?<!--<CR>:s/^\( *\)\%(\(<\)!-- \?\(\/\?[^<]\+>\)\\|<!-- \?\)/\1\2\3/e<CR>/--><CR>:s/\(<\/\?[a-zA-Z]\+\) \?--\(>\)\\| \?-->/\1\2/e<CR>:noh<CR>0j
au BufNewFile,BufRead,BufEnter *.html,*.st vmap <buffer> - <C-C>`>a --<C-V>><Esc>`<i<!-- <Esc>

runtime ftplugin/man.vim
set keywordprg=:Man

" haskell
au BufNewFile,BufRead,BufEnter *.hs,.ghci setlocal formatprg=hindent\ --tab-size\ 2\ -XQuasiQuotes
au BufNewFile,BufRead,BufEnter *.hs,.ghci setlocal makeprg=stack\ build
au BufNewFile,BufRead,BufEnter *.hs,.ghci setlocal keywordprg=hoogle\ --info
au BufNewFile,BufRead,BufEnter *.hs,.ghci map <silent> gl :cex system('hlint .')<CR>
au BufNewFile,BufRead,BufEnter *.hs,.ghci runtime ftplugin/haskell.vim
au BufNewFile,BufRead,BufEnter *.hs,.ghci runtime ext/haskell.vim
au BufNewFile,BufRead,BufEnter *.hs,.ghci nnoremap <silent> <F1> :terminal ++close ++rows=10 ghci %<CR>
au BufReadPost .ghci* set syntax=haskell

" switch directly to ghci terminal or run one if doesn't exist (incomplete)
" function! FindGHCI()
"   let terms = filter(range(1, bufnr('$')), 'bufexists(v:val) && getbufvar(v:val, "ghci_term", 0)')
" endfunction

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

au BufNewFile,BufRead,BufEnter *.hs,.ghci map <silent> to :call ApplyOneSuggestion()<CR>
au BufNewFile,BufRead,BufEnter *.hs,.ghci map <silent> ta :call ApplyAllSuggestions()<CR>

" function! CommittedFiles()
"   " Clear quickfix list
"   let qf_list = []
"   " Find files committed in HEAD
"   let git_output = system("git diff-tree --no-commit-id --name-only -r HEAD\n")
"   for committed_file in split(git_output, "\n")
"     let qf_item = {'filename': committed_file}
"     call add(qf_list, qf_item)
"   endfor
"   " Fill quickfix list with them
"   call setqflist(qf_list)
" endfunction

" " Show list of last-committed files
" nnoremap <silent> <leader>g? :call CommittedFiles()<CR>:copen<CR>
