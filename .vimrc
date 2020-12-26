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
  set complete=.,t,d,i,k~/.vim/spell/en.utf-8.add
  return ''
endfunction
function! DoTabPost()
  set complete=.,w,b,u,t,d,i,kspell
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
set wrap magic number relativenumber ruler showmode tw=80
set lazyredraw notimeout ttimeout timeoutlen=100
nnoremap <silent> j gj
nnoremap <silent> gj j
nnoremap <silent> k gk
nnoremap <silent> gk k
nnoremap <silent> <F9> :make<CR>
inoremap <silent> <F9> <ESC>:w<CR>:make<CR>
" don't run make unless explicitly set by file type
set makeprg=/usr/bin/true
" swap last deleted text with visually-highlighted text
vnoremap <silent> <C-x> <ESC>:undojoin<CR>`.``gv""p``P

" turn on folding
set foldmethod=syntax foldlevelstart=4

" don't wrap lines when editing
set formatoptions-=t

" remove comment leader when joining comment lines
set formatoptions+=j

" turn off bells
set noerrorbells vb t_vb=

if &term =~ '256color'
  " disable Background Color Erase (BCE)
  " see http://snk.tuxfamily.org/log/vim-256color-bce.html
  set t_ut=
endif

set smarttab smartindent autoindent softtabstop=2 tabstop=2 shiftwidth=2 expandtab

" change cursor to a vertical line when in editing mode
set t_Co=256
let &t_SI = "\<ESC>]50;CursorShape=1\x7"
let &t_EI = "\<ESC>]50;CursorShape=0\x7"
hi! CursorLine ctermbg=000
hi! CursorColumn ctermbg=000

" search options
set smartcase hlsearch incsearch
hi Search ctermfg=white ctermbg=173 cterm=none guifg=#ffffff guibg=#e5786d gui=none

" backspace q to quit, bs bs to quit all, if possible
nnoremap <silent> <BS><BS> :qa<CR>
nnoremap <silent> <BS><SPACE> ZZ
nnoremap <silent> <BS>q :q<CR>
nnoremap <silent> <BS>Q :q!<CR>
nnoremap <silent> <BS>! :q!<CR>

set mouse=
nnoremap <silent> zma :set mouse=a<CR>
nnoremap <silent> zmn :set mouse=n<CR>
nnoremap <silent> zmo :set mouse=<CR>
set history=1000
set backspace=indent,eol,start
set splitbelow splitright

augroup gitstatusline
  autocmd!
  autocmd BufEnter,FocusGained,BufWrite,BufWritePost <buffer> let b:git_status=system("git_status " . expand("%:p"))
augroup end

" status line
set laststatus=2
function MyStatusLine()
  let l:sl="%n "
  let l:sl.="%<%f"
  let l:sl.=" %h%m%r"
  let l:sl.="%=%1.20("
  if !exists("g:statusline_winid")
    " Vim < 8.2 e.g. Debian buster
    let l:buf=bufnr("%")
  else
    let l:buf=winbufnr(g:statusline_winid)
  endif
  let l:sl.=getbufvar(l:buf, "git_status")
  let l:sl.="%)  %-8.(%l,%c%V%) %P "
  return l:sl
endfunction
set statusline=%!MyStatusLine()
function! ModifiedColor()
  if &mod == 1
    hi StatusLine ctermfg=58 ctermbg=8
    hi GitGreen ctermfg=040 ctermbg=58
    hi GitBlue ctermfg=004 ctermbg=58
    hi GitRed ctermfg=400 ctermbg=58
    hi GitYellow cterm=bold ctermfg=440 ctermbg=58
  else
    if &ro == 1
      hi StatusLine ctermfg=23 ctermbg=8
      hi GitGreen ctermfg=040 ctermbg=23
      hi GitBlue ctermfg=004 ctermbg=23
      hi GitRed ctermfg=400 ctermbg=23
      hi GitYellow cterm=bold ctermfg=440 ctermbg=23
    else
      hi StatusLine ctermfg=24 ctermbg=8
      hi GitGreen ctermfg=040 ctermbg=24
      hi GitBlue ctermfg=004 ctermbg=24
      hi GitRed ctermfg=400 ctermbg=24
      hi GitYellow cterm=bold ctermfg=440 ctermbg=24
    endif
  endif
endfunction
au BufEnter,BufLeave,TextChanged,TextChangedI,TextChangedP,BufWritePost * call ModifiedColor()
hi StatusLine ctermfg=24 ctermbg=8
hi StatusLineNC ctermfg=237 ctermbg=8
hi StatusLineTerm cterm=bold,reverse ctermfg=22 ctermbg=8
hi StatusLineTermNC cterm=bold,reverse ctermfg=237 ctermbg=8
hi GitGreen ctermfg=040 ctermbg=8
hi GitBlue ctermfg=004 ctermbg=8
hi GitRed ctermfg=400 ctermbg=8
hi GitYellow cterm=bold ctermfg=440 ctermbg=8

nnoremap <silent> <F12> :set invnumber invrelativenumber mouse=<CR>
nnoremap <silent> <F10> :term git -P lola<CR>
nnoremap <silent> gr :term ++close git rebase -i <cword><CR>
nnoremap <silent> gs :term ++close ++kill=term git show <cword><CR>
nnoremap <silent> gA :silent call system("git -C " . expand ("%:h") . " add -f " . expand("%:t"))<CR>:let b:git_status=system("git_status " . expand("%:p"))<CR>

if system('uname -s') == "Darwin\n"
"   set clipboard=unnamed,autoselect "OSX
  let &t_ZH="\e[3m"
  let &t_ZR="\e[23m"
else
  set clipboard=unnamedplus,autoselect "Linux
endif

" search into subfolders and with wildcards for better tab-completion
set path+=**
set wildignore+=*/tmp/*,*.swp,*.swo,*.zip,.git,.cabal-sandbox
set wildmode=list:longest,full wildmenu

" tab to turn off search highlighting and show status
nnoremap <silent> <TAB> :noh<CR><C-g>

" space to save, ctrl-space to save and switch to next window, shift-tab to just switch windows
nnoremap <silent> <SPACE> :w<CR>
nnoremap <silent> <C-@> :w<CR><C-w>w
nnoremap <silent> <C-SPACE> :w<CR><C-w>w
nnoremap <silent> <S-TAB> <C-w>p
tnoremap <silent> <F1> <C-w>N
tnoremap <silent> <S-TAB> <C-w>p
tnoremap <silent> <C-v> <C-w>""

" Q to record q macro, return to run q macro, M to set m mark
nnoremap <silent> Q qq
nnoremap <silent> <CR> @q
nnoremap <silent> M mm
" highlight the m marker - doesn't update when marker changed
" match Error /\%'m/

" quickfix open, up, down, next file
map <silent> <C-h> :cw<CR>
map <silent> <C-j> :cn<CR>
map <silent> <C-k> :cp<CR>
map <silent> <C-S-j> :cnf<CR>

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
nnoremap <silent> <S-UP> :resize +2<CR>
nnoremap <silent> <S-DOWN> :resize -2<CR>
nnoremap <silent> <S-LEFT> :vertical resize +2<CR>
nnoremap <silent> <S-RIGHT> :vertical resize -2<CR>
tnoremap <silent> <S-UP> <C-w>:resize +2<CR>
tnoremap <silent> <S-DOWN> <C-w>:resize -2<CR>
tnoremap <silent> <S-LEFT> <C-w>:vertical resize +2<CR>
tnoremap <silent> <S-RIGHT> <C-w>:vertical resize -2<CR>

nnoremap <silent> zh <C-W>h
nnoremap <silent> zj <C-W>j
nnoremap <silent> zk <C-W>k
nnoremap <silent> zl <C-W>l
nnoremap <silent> zH <C-W>H
nnoremap <silent> zJ <C-W>J
nnoremap <silent> zK <C-W>K
nnoremap <silent> zL <C-W>L

" number of lines to keep below cursor when scrolling
set scrolloff=15

" show trailing whitespace
set list
if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

" change word under cursor and find next occurrence of that word
nnoremap <silent> cn *``cgn
nnoremap <silent> cN *``cgN

" add surrounding characters around word or selection
" nnoremap <silent> <C-e><C-e> 10<C-e>
nnoremap <silent> <C-e>l Lzz
nnoremap <silent> <C-e>e ciW(<c-r><c-o>")<ESC>
nnoremap <silent> <C-e>a ciW<<c-r><c-o>"><ESC>
nnoremap <silent> <C-e>s ciW[<c-r><c-o>"]<ESC>
inoremap <silent> <C-e>e <ESC>ciW(<c-r><c-o>")
inoremap <silent> <C-e>a <ESC>ciW<<c-r><c-o>">
inoremap <silent> <C-e>s <ESC>ciW[<c-r><c-o>"]
vnoremap <silent> <C-e>e <ESC>`>a)<ESC>`<i(<ESC>
vnoremap <silent> <C-e>a <ESC>`>a><ESC>`<i<<ESC>
vnoremap <silent> <C-e>s <ESC>`>a]<ESC>`<is<ESC>

" make <C-y> and <C-e><C-e> work word by word
imap <expr> <C-y> matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\s*\S\+\)')
imap <expr> <C-e><C-e> matchstr(getline(line('.')+1), '\%' . virtcol('.') . 'v\%(\s*\S\+\)')
" <C-y> followed by single-char movement, like $ or E
" inoremap <silent> <C-y> <SPACE><C-o>:exec "normal! khy" . nr2char(getchar()) . "`.\"_xp\|A"<CR>
" inoremap <silent> <C-y> <SPACE><ESC>k:set opfunc=CopyAbove<CR>g@<CR>jpa
" function! CopyAbove(type)
"   silent exe "normal! `[v`]y"
" endfunction

" add/remove comments in various file formats
au BufNewFile,BufRead,BufEnter * map <silent> - @='gI# <C-V><ESC>0j'<CR>
au BufNewFile,BufRead,BufEnter * map <silent> _ :s/^\( *\)# \?/\1/e<CR>:noh<CR>0j
au BufNewFile,BufRead,BufEnter .vimrc,*.vim map <silent> - @='gI" <C-V><ESC>0j'<CR>
au BufNewFile,BufRead,BufEnter .vimrc,*.vim map <silent> _ :s/^\( *\)" \?/\1/e<CR>:noh<CR>0j
au BufNewFile,BufRead,BufEnter *.c,*.h,*.cpp,*.java,*.js map <silent> - @='gI// <C-V><ESC>0j'<CR>
au BufNewFile,BufRead,BufEnter *.c,*.h,*.cpp,*.java,*.js map <silent> _ :s/^\( *\)\/\/ \?/\1/e<CR>:noh<CR>0j
au BufNewFile,BufRead,BufEnter *.c,*.h,*.cpp,*.java,*.js vmap <buffer> - <C-C>`>a */<ESC>`<i/* <ESC>
au BufNewFile,BufRead,BufEnter *.c,*.h,*.cpp setlocal makeprg=make
au BufNewFile,BufRead,BufEnter *.hs,.ghci*,*.cabal map <silent> - @='gI-- <C-V><ESC>0j'<CR>
au BufNewFile,BufRead,BufEnter *.hs,.ghci*,*.cabal map <silent> _ :s/^\( *\)-- \?/\1/e<CR>:noh<CR>0j
au BufNewFile,BufRead,BufEnter *.hs,.ghci*,*.cabal vmap <buffer> - <C-C>`>a -}<ESC>`<i{- <ESC>
au BufNewFile,BufRead,BufEnter *.lhs map <silent> - :s/^> /> -- /e<CR>:noh<CR>0j
au BufNewFile,BufRead,BufEnter *.lhs map <silent> _ :s/^> -- /> /e<CR>:noh<CR>0j
au BufNewFile,BufRead,BufEnter *.html,*.st map <silent> - @='gI<!-- <C-V><ESC>A --><C-V><ESC>0j'<CR>
au BufNewFile,BufRead,BufEnter *.html,*.st map <silent> _ /--><CR>?<!--<CR>:s/^\( *\)\%(\(<\)!-- \?\(\/\?[^<]\+>\)\\|<!-- \?\)/\1\2\3/e<CR>/--><CR>:s/\(<\/\?[a-zA-Z]\+\) \?--\(>\)\\| \?-->/\1\2/e<CR>:noh<CR>0j
au BufNewFile,BufRead,BufEnter *.html,*.st vmap <buffer> - <C-C>`>a --<C-V>><ESC>`<i<!-- <ESC>

" other auto commands
au InsertLeave * set nopaste
au BufWritePost */spell/*.add silent! :mkspell! %

runtime ftplugin/man.vim
set keywordprg=:Man

" add concealment to markdown code
au BufNewFile,BufRead,BufEnter *.md,*.markdown syn region markdownCode matchgroup=markdownCodeDelimiter start="`" end="`" keepend contains=markdownLineStart concealends
au BufNewFile,BufRead,BufEnter *.md,*.markdown syn region markdownCode matchgroup=markdownCodeDelimiter start="`` \=" end=" \=``" keepend contains=markdownLineStart concealends
au BufNewFile,BufRead,BufEnter *.md,*.markdown syn region markdownCode matchgroup=markdownCodeDelimiter start="^\s*```.*$" end="^\s*```\ze\s*$" keepend concealends
au BufNewFile,BufRead,BufEnter *.md,*.markdown hi markdownCode ctermfg=248 ctermbg=237
au BufNewFile,BufRead,BufEnter *.md,*.markdown set cole=0
au BufNewFile,BufRead,BufEnter *.md,*.markdown nnoremap <silent> <F1> :call ConcealToggle()<CR>
function! ConcealToggle()
  if &cole
    setlocal cole=0
  else
    setlocal cole=2
  endif
endfunction

" haskell
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* setlocal formatprg=hindent\ --tab-size\ 2\ -XQuasiQuotes
" au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* setlocal makeprg=stack\ build
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* setlocal makeprg=make
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* setlocal keywordprg=hoogle-info
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* map <silent> gl :cex system('hlint .')<CR>
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* runtime ftplugin/haskell.vim
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* runtime ext/haskell.vim
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* nnoremap <silent> <F1> :term ++close ++rows=15 ++kill=term ghci -v0 %<CR><C-w>p
au BufNewFile,BufRead,BufEnter *.lhs,*.hs nnoremap <silent> <C-@> :w<CR>:call term_sendkeys('ghci ', ":r\n")<CR><C-w>j
au BufNewFile,BufRead,BufEnter *.lhs,*.hs nnoremap <silent> <C-SPACE> :w<CR>:call term_sendkeys('ghci ', ":r\n")<CR><C-w>j
" au BufWritePost *.lhs,*.hs silent call term_sendkeys('ghci ', ":r\n")
" au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* let ghcidprg = "ghci"
" au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* nnoremap <silent> <F3> :vertical :term ++close ++cols=75 ++kill=int ghcid -c <C-R>=ghcidprg<CR> %<CR><C-w>p
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* nnoremap <silent> <F3> :vertical :term ++close ++cols=75 ++kill=int ghcid %<CR><C-w>p
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* nnoremap <silent> <F4> :vertical :term ++close ++cols=75 ++kill=int watch make test<CR><C-w>p
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* inoremap <silent> <C-]><C-]> <SPACE>-><SPACE>
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* inoremap <silent> <C-]>[ <SPACE><-<SPACE>
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* inoremap <silent> <C-]>] <SPACE>=><SPACE>
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* inoremap <silent> <C-]>: <SPACE>∷<SPACE>
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* inoremap <silent> <C-]>. <SPACE>→<SPACE>
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* inoremap <silent> <C-]>> <SPACE>⇒<SPACE>
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab -] ->
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab [- <-
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab =] =>
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab [= <=
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab MI Maybe Int
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab MII Maybe (Int -> Int)
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab IMI (Int -> Maybe Int)
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab LI [Int]
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab LII [Int -> Int]
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab ILI (Int -> [Int])
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab Sg String
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab LS [String]
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab MS Maybe String
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab SyE import System.Environment
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab SyI import System.IO
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab PPi import PP<CR><CR>main = interact
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab DM import qualified Data.Map as M
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab DIM import qualified Data.IntMap as IM
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab DS import qualified Data.Set as S
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab TP import Text.Parsec
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab denum deriving (Show, Eq, Read, Ord, Bounded, Enum)
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* nnoremap <buffer> <silent> gs 0yiWO<ESC>pA<SPACE>::<SPACE>
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

au BufNewFile,BufRead,BufEnter *.hs,.ghci* map <silent> to :call ApplyOneSuggestion()<CR>
au BufNewFile,BufRead,BufEnter *.hs,.ghci* map <silent> ta :call ApplyAllSuggestions()<CR>

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

" Plugins

call plug#begin('~/.vim/plugged')
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'
Plug 'thomasfaingnaert/vim-lsp-snippets'
Plug 'thomasfaingnaert/vim-lsp-ultisnips'
call plug#end()

let g:UltiSnipsExpandTrigger="<TAB>"
let g:UltiSnipsJumpForwardTrigger="<TAB>"
let g:UltiSnipsJumpBackwardTrigger="<S-TAB>"

" vim-lsp

if executable('pyls')
    " pip install python-language-server
    au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'allowlist': ['python'],
        \ })
endif

if executable('hls')
  au User lsp_setup call lsp#register_server({
      \ 'name': 'hls',
      \ 'cmd': {server_info->['hls', '--lsp']},
      \ 'whitelist': ['haskell'],
      \ })
endif

function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
    setlocal signcolumn=yes
    if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
    nmap <buffer> gd <plug>(lsp-definition)
    nmap <buffer> gr <plug>(lsp-references)
    nmap <buffer> gf <plug>(lsp-code-action)
    nmap <buffer> gi <plug>(lsp-implementation)
    nmap <buffer> gt <plug>(lsp-type-definition)
    nmap <buffer> <F2> <plug>(lsp-rename)
    nmap <buffer> [g <Plug>(lsp-previous-diagnostic)
    nmap <buffer> ]g <Plug>(lsp-next-diagnostic)
    nmap <buffer> gG :LspDocumentDiagnostics<CR>
    nmap <buffer> <S-C-k> <plug>(lsp-hover)
"     nmap <buffer> f <plug>(lsp-document-range-format)
    nmap <buffer> <F5> <plug>(lsp-code-lens)

    " refer to doc to add more commands
    " set foldmethod=expr
    "   \ foldexpr=lsp#ui#vim#folding#foldexpr()
    "   \ foldtext=lsp#ui#vim#folding#foldtext()
endfunction

augroup lsp_install
    au!
    " call s:on_lsp_buffer_enabled only for languages that has the server registered.
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()

    let g:lsp_hover_conceal = 0

    let g:lsp_signs_error = {'text': '✗'}
    let g:lsp_signs_warning = {'text': '‼'}
    let g:lsp_signs_information = {'text': '-'}
    let g:lsp_signs_hint = {'text': '!'}

    let g:lsp_highlight_references_enabled = 1
    highlight lspReference term=italic,bold ctermbg=238 gui=italic,bold
    highlight lspErrorText term=italic,bold ctermbg=238 gui=italic,bold
augroup END
