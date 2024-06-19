set encoding=utf-8
set fileencoding=utf-8

" source the vimrc file after saving it
autocmd! bufwritepost ~/.vimrc source $MYVIMRC

" turn on syntax highlighting, with slate colorscheme
syntax enable
colorscheme default
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

" file navigator
let g:netrw_banner=0
let g:netrw_browse_split=4 " open in prior window
let g:netrw_altv=1         " open splits to the right
let g:netrw_liststyle=3    " tree view
let g:netrw_list_hide=netrw_gitignore#Hide()
let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

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
xnoremap <silent> <C-x> <ESC>:undojoin<CR>`.``gv""p``P
" repeat . over a range
xnoremap <silent> . :normal .<CR>

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
nnoremap <RightMouse> "+]p
set history=1000
set backspace=indent,eol,start
set splitbelow splitright

augroup statusline
  autocmd!
  autocmd BufEnter,BufWrite,BufWritePost <buffer> let b:git_status=system("git_status " . expand("%:p"))
augroup end

" status line
set laststatus=2
function MyStatusLine()
  if !exists("g:statusline_winid")
    " Vim < 8.2 e.g. Debian buster, doesn't quite work properly with splits, but prevents errors
    let l:buf=bufnr("%")
    let l:win=win_getid()
  else
    let l:buf=winbufnr(g:statusline_winid)
    let l:win=g:statusline_winid
  endif

  if l:win == win_getid()
    if getbufvar(l:buf, "&mod") == 1
      let l:mystatus="Mod"
    else
      if getbufvar(l:buf, "&ro") == 1
        let l:mystatus="RO"
      else
        let l:mystatus="OK"
      endif
    endif
  else
    let l:mystatus="Inactive"
  endif

  let l:sl="%n "
  let l:sl.="%<%f "
  let l:sl.=getbufvar(l:buf, "&buftype")
" Filetype-specific glyphs (alternatives: ﬦ)
  let l:sl.=get({"vim": "%#VimGreen# %*"
               \,"haskell": "%#HaskellPurple# %*"
               \,"javascript": "%#JSYellow# %*"
               \,"make": "%#White# %*"
               \,"html": "%#HtmlOrange# %*"
               \,"python": "%#JSYellow# %*"
               \,"elm": "%#ElmBlue# %*"
               \}, getbufvar(l:buf, "&ft"), "")
  let l:sl.="%h%m%r"
  let l:sl.="%=%1.20("
  let l:sl.=getbufvar(l:buf, "git_status")
  let l:sl.="%)"
  let l:sl.="  %-8.(%l,%c%V%) %P "
  return substitute(l:sl, '\(%#\a\+\)#', '\1' . l:mystatus . "#", "g")
endfunction
set statusline=%!MyStatusLine()

au BufNewFile,BufRead,BufEnter,TextChanged,TextChangedI,TextChangedP,BufWritePost * call ModifiedColor()
function ModifiedColor()
  if &mod == 1
    hi StatusLine ctermfg=58 ctermbg=110
  else
    if &ro == 1
      hi StatusLine ctermfg=23 ctermbg=110
    else
      hi StatusLine ctermfg=24 ctermbg=110
    endif
  endif
endfunction

hi StatusLine ctermfg=24 ctermbg=110
hi StatusLineNC ctermfg=237 ctermbg=110
hi StatusLineTerm cterm=bold,reverse ctermfg=22 ctermbg=110
hi StatusLineTermNC cterm=bold,reverse ctermfg=237 ctermbg=110

hi GitGreenOK ctermfg=040 ctermbg=24
hi GitGreenMod ctermfg=040 ctermbg=58
hi GitGreenRO ctermfg=040 ctermbg=23
hi GitGreenInactive ctermfg=040 ctermbg=237
hi GitBlueOK ctermfg=004 ctermbg=24
hi GitBlueMod ctermfg=004 ctermbg=58
hi GitBlueRO ctermfg=004 ctermbg=23
hi GitBlueInactive ctermfg=004 ctermbg=237
hi GitYellowOK cterm=bold ctermfg=440 ctermbg=24
hi GitYellowMod cterm=bold ctermfg=440 ctermbg=58
hi GitYellowRO cterm=bold ctermfg=440 ctermbg=23
hi GitYellowInactive cterm=bold ctermfg=440 ctermbg=237
hi GitRedOK ctermfg=400 ctermbg=24
hi GitRedMod ctermfg=400 ctermbg=58
hi GitRedRO ctermfg=400 ctermbg=23
hi GitRedInactive ctermfg=400 ctermbg=237
hi HaskellPurpleOK cterm=bold ctermfg=129 ctermbg=24
hi HaskellPurpleMod cterm=bold ctermfg=129 ctermbg=58
hi HaskellPurpleRO cterm=bold ctermfg=129 ctermbg=23
hi HaskellPurpleInactive cterm=bold ctermfg=129 ctermbg=237
hi JSYellowOK ctermfg=221 ctermbg=24
hi JSYellowMod ctermfg=221 ctermbg=58
hi JSYellowRO ctermfg=221 ctermbg=23
hi JSYellowInactive ctermfg=221 ctermbg=237
hi VimGreenOK ctermfg=040 ctermbg=24
hi VimGreenMod ctermfg=040 ctermbg=58
hi VimGreenRO ctermfg=040 ctermbg=23
hi VimGreenInactive ctermfg=040 ctermbg=237
hi HtmlOrangeOK ctermfg=166 ctermbg=24
hi HtmlOrangeMod ctermfg=166 ctermbg=58
hi HtmlOrangeRO ctermfg=166 ctermbg=23
hi HtmlOrangeInactive ctermfg=166 ctermbg=237
hi ElmBlueOK cterm=bold ctermfg=32 ctermbg=24
hi ElmBlueMod cterm=bold ctermfg=32 ctermbg=58
hi ElmBlueRO cterm=bold ctermfg=32 ctermbg=23
hi ElmBlueInactive cterm=bold ctermfg=32 ctermbg=237
hi WhiteOK cterm=bold ctermfg=231 ctermbg=24
hi WhiteMod cterm=bold ctermfg=231 ctermbg=58
hi WhiteRO cterm=bold ctermfg=231 ctermbg=23
hi WhiteInactive cterm=bold ctermfg=231 ctermbg=237

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
map <silent> <C-h> :cw<CR>z.
map <silent> <C-j> :cn<CR>z.
map <silent> <C-k> :cp<CR>z.
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
nnoremap <silent> <C-e>e ciW(<C-r><C-o>")<ESC>
nnoremap <silent> <C-e>a ciW<<C-r><C-o>"><ESC>
nnoremap <silent> <C-e>s ciW[<C-r><C-o>"]<ESC>
inoremap <silent> <C-e>e <ESC>ciW(<C-r><C-o>")
inoremap <silent> <C-e>a <ESC>ciW<<C-r><C-o>">
inoremap <silent> <C-e>s <ESC>ciW[<C-r><C-o>"]
vnoremap <silent> <C-e>e <ESC>`>a)<ESC>`<i(<ESC>
vnoremap <silent> <C-e>a <ESC>`>a><ESC>`<i<<ESC>
vnoremap <silent> <C-e>s <ESC>`>a]<ESC>`<is<ESC>

" make tuples from parenthesized expressions or from words
nnoremap <silent> Mt% ya)pa, (<ESC>%a)<ESC>
nnoremap <silent> Mtw ciW(<c-r><c-o>", <c-r><c-o>")<ESC>

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
au BufNewFile,BufRead,BufEnter * map <silent> _ :keeppatterns s/^\( *\)# \?/\1/e<CR>0j:noh<CR>
au BufNewFile,BufRead,BufEnter .vimrc,*.vim map <silent> - @='gI" <C-V><ESC>0j'<CR>
au BufNewFile,BufRead,BufEnter .vimrc,*.vim map <silent> _ :keeppatterns s/^\( *\)" \?/\1/e<CR>0j:noh<CR>
au BufNewFile,BufRead,BufEnter *.c,*.h,*.cpp,*.java,*.js map <silent> - @='gI// <C-V><ESC>0j'<CR>
au BufNewFile,BufRead,BufEnter *.c,*.h,*.cpp,*.java,*.js map <silent> _ :keeppatterns s/^\( *\)\/\/ \?/\1/e<CR>0j:noh<CR>
au BufNewFile,BufRead,BufEnter *.c,*.h,*.cpp,*.java,*.js vmap <buffer> - <C-C>`>a */<ESC>`<i/* <ESC>
au BufNewFile,BufRead,BufEnter *.c,*.h,*.cpp setlocal makeprg=make
au BufNewFile,BufRead,BufEnter *.elm,*.hs,.ghci*,*.cabal,**/.cabal/config map <silent> - @='gI-- <C-V><ESC>0j'<CR>
au BufNewFile,BufRead,BufEnter *.elm,*.hs,.ghci*,*.cabal,**/.cabal/config map <silent> _ :keeppatterns s/^\( *\)-- \?/\1/e<CR>0j:noh<CR>
au BufNewFile,BufRead,BufEnter *.elm,*.hs,.ghci*,*.cabal,**/.cabal/config vmap <buffer> - <C-C>`>a -}<ESC>`<i{- <ESC>
au BufNewFile,BufRead,BufEnter *.lhs map <silent> - :keeppatterns s/^> /> -- /e<CR>0j:noh<CR>
au BufNewFile,BufRead,BufEnter *.lhs map <silent> _ :keeppatterns s/^> -- /> /e<CR>0j:noh<CR>
au BufNewFile,BufRead,BufEnter *.html,*.st map <silent> - @='gI<!-- <C-V><ESC>A --><C-V><ESC>0j'<CR>
au BufNewFile,BufRead,BufEnter *.html,*.st map <silent> _ :keeppatterns /--><CR>:keeppatterns ?<!--<CR>:keeppatterns s/^\( *\)\%(\(<\)!-- \?\(\/\?[^<]\+>\)\\|<!-- \?\)/\1\2\3/e<CR>:keeppatterns /--><CR>:keeppatterns s/\(<\/\?[a-zA-Z]\+\) \?--\(>\)\\| \?-->/\1\2/e<CR>0j:noh<CR>
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

" silence gq
" nnoremap <silent> gq :setlocal opfunc=FormatPrg<cr>g@

" function! FormatPrg(...)
"   silent exe "'[,']!".&formatprg
" endfunction

" haskell
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* setlocal formatprg=stylish-haskell
" au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* setlocal makeprg=stack\ build
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci*,*.elm setlocal makeprg=make
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* setlocal keywordprg=hoogle-info
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* noremap <silent> K <Cmd>call ReadMan(expand('<cword>'), "Haskell")<CR>
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* setlocal iskeyword+=@-@,',$,<->,!,\|,/,~,%,94,*,+,&,_
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
" move by "section" (function)
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* nnoremap <silent> [[ ?^[a-z].*=[^>]<CR>:call RestoreSearch()<CR>:noh<CR>
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* nnoremap <silent> ]] /^[a-z].*=[^>]<CR>:call RestoreSearch()<CR>:noh<CR>
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* nnoremap <silent> ][ /^[a-z].*=[^>]<CR>:call RestoreSearch()<CR>:keeppatterns<SPACE>?[a-z]<CR>:noh<CR>
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* nnoremap <silent> [] /^[a-z].*=[^>]<CR>:call RestoreSearch()<CR>:keeppatterns<SPACE>/[a-z]<CR>:noh<CR>
" move to import section
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* nnoremap <silent> [i :$?^import <CR>:call RestoreSearch()<CR>:noh<CR>
" move to main definition
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* nnoremap <silent> [m /^main *=<CR>:call RestoreSearch()<CR>:noh<CR>
" move to = on main
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* nnoremap <silent> M [mf=
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
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab Str String
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab LStr [String]
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab MStr Maybe String
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab iDL import Data.List
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab iDC import Data.Char
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab iSE import System.Environment
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab iSI import System.IO
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab iPP import PP<CR><CR>main = interact
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab iDM import qualified Data.Map as M
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab iDIM import qualified Data.IntMap as IM
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab iDS import qualified Data.Set as S
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab iTP import Text.Parsec
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* ab denum deriving (Show, Eq, Read, Ord, Bounded, Enum)
au BufNewFile,BufRead,BufEnter *.lhs,*.hs,.ghci* nnoremap <buffer> <silent> gs ^yiWO<ESC>pA<SPACE>::<SPACE>
au BufReadPost .ghci* set syntax=haskell

function! RestoreSearch()
  call histdel("search", -1)
  let @/ = histget("search", -1)
endfunction

" switch directly to ghci terminal or run one if doesn't exist (incomplete)
" function! FindGHCI()
"   let terms = filter(range(1, bufnr('$')), 'bufexists(v:val) && getbufvar(v:val, "ghci_term", 0)')
" endfunction

" https://github.com/mpickering/hlint-refactor-vim
function! ApplyOneSuggestion()
  let l:l = line(".")
  let l:c = col(".")
  let l:filter = "%!hlint - --refactor --refactor-options=\"--pos ".l:l.','.l:c."\""
  silent execute l:filter
  call cursor(l:l, l:c)
endfunction

function! ApplyAllSuggestions()
  let l:l = line(".")
  let l:c = col(".")
  let l:filter = "%!hlint - --refactor"
  silent execute l:filter
  call cursor(l:l, l:c)
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

if filereadable(expand('~/.vim/autoload/plug.vim'))
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
endif

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

if executable('haskell-language-server')
  au User lsp_setup call lsp#register_server({
      \ 'name': 'haskell-language-server',
      \ 'cmd': {server_info->['haskell-language-server', '--lsp']},
      \ 'whitelist': ['haskell'],
      \ })
endif

function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
    setlocal signcolumn=yes
    if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
    nmap <buffer> Ld <plug>(lsp-definition)
    nmap <buffer> Lr <plug>(lsp-references)
    nmap <buffer> Lf <plug>(lsp-code-action)
    nmap <buffer> Li <plug>(lsp-implementation)
    nmap <buffer> Lt <plug>(lsp-type-definition)
    nmap <buffer> <F2> <plug>(lsp-rename)
    nmap <buffer> [g <plug>(lsp-previous-diagnostic)
    nmap <buffer> ]g <plug>(lsp-next-diagnostic)
    nmap <buffer> gG <plug>(lsp-document-diagnostics)
    nmap <buffer> LL <plug>(lsp-hover)
"     nmap <buffer> f <plug>(lsp-document-range-format)
    nmap <buffer> Ll <plug>(lsp-code-lens)

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

    let g:lsp_document_code_action_signs_enabled = 0
    let g:lsp_diagnostics_signs_error = {'text': '✗'}
    let g:lsp_diagnostics_signs_warning = {'text': '‼'}
    let g:lsp_diagnostics_signs_information = {'text': '-'}
    let g:lsp_diagnostics_signs_hint = {'text': '!'}

    highlight lspReference term=italic,bold ctermbg=238 gui=italic,bold
    highlight lspErrorText term=italic,bold ctermbg=238 gui=italic,bold
augroup END

function! ReadMan(word, ft)
  let prg = &l:keywordprg
  execute ":wincmd n"
  execute ":setlocal buftype=nofile"
  execute ":setlocal bufhidden=hide"
  execute ":setlocal noswapfile"
  execute ":setlocal nobuflisted"
  execute ":r!" . prg . " '" . a:word . "'"
  execute ":set ft=" . a:ft
  execute ":goto"
  execute ":delete"
endfunction

function! LineUpOnEquals()
endfunction

" The following is from
" https://github.com/michaeljsmith/vim-indent-object/blob/master/plugin/indent-object.vim
" with some patches applied.
"--------------------------------------------------------------------------------
"
"  Copyright (c) 2010 Michael Smith <msmith@msmith.id.au>
"
"  http://github.com/michaeljsmith/vim-indent-object
"
"  Permission is hereby granted, free of charge, to any person obtaining a copy
"  of this software and associated documentation files (the "Software"), to
"  deal in the Software without restriction, including without limitation the
"  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
"  sell copies of the Software, and to permit persons to whom the Software is
"  furnished to do so, subject to the following conditions:
"
"  The above copyright notice and this permission notice shall be included in
"  all copies or substantial portions of the Software.
"
"  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
"  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
"  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
"  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
"  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
"  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
"  IN THE SOFTWARE.
"
"--------------------------------------------------------------------------------

" Mappings excluding line below.
onoremap <silent>ai :<C-u>cal HandleTextObjectMapping(0, 0, 0, [line("."), line("."), col("."), col(".")])<CR>
onoremap <silent>ii :<C-u>cal HandleTextObjectMapping(1, 0, 0, [line("."), line("."), col("."), col(".")])<CR>
xnoremap <silent>ai :<C-u>cal HandleTextObjectMapping(0, 0, 1, [line("'<"), line("'>"), col("'<"), col("'>")])<CR><Esc>gv
xnoremap <silent>ii :<C-u>cal HandleTextObjectMapping(1, 0, 1, [line("'<"), line("'>"), col("'<"), col("'>")])<CR><Esc>gv

" Mappings including line below.
onoremap <silent>aI :<C-u>cal HandleTextObjectMapping(0, 1, 0, [line("."), line("."), col("."), col(".")])<CR>
onoremap <silent>iI :<C-u>cal HandleTextObjectMapping(1, 1, 0, [line("."), line("."), col("."), col(".")])<CR>
xnoremap <silent>aI :<C-u>cal HandleTextObjectMapping(0, 1, 1, [line("'<"), line("'>"), col("'<"), col("'>")])<CR><Esc>gv
xnoremap <silent>iI :<C-u>cal HandleTextObjectMapping(1, 1, 1, [line("'<"), line("'>"), col("'<"), col("'>")])<CR><Esc>gv

let s:l0 = -1
let s:l1 = -1
let s:c0 = -1
let s:c1 = -1

if !exists("g:indent_object_except_first_level")
  let g:indent_object_except_first_level = 1
endif

function! TextObject(inner, incbelow, vis, range, count)

  if exists("g:indent_object_include_blanks")
    let include_blanks = g:indent_object_include_blanks
  else
    let include_blanks = 0
  endif

  " Record the current state of the visual region.
  let vismode = "V"

  " Detect if this is a completely new visual selction session.
  let new_vis = 0
  let new_vis = new_vis || s:l0 != a:range[0]
  let new_vis = new_vis || s:l1 != a:range[1]
  let new_vis = new_vis || s:c0 != a:range[2]
  let new_vis = new_vis || s:c1 != a:range[3]

  let s:l0 = a:range[0]
  let s:l1 = a:range[1]
  let s:c0 = a:range[2]
  let s:c1 = a:range[3]

  " Repeatedly increase the scope of the selection.
  let itr_cnt = 0
  let cnt = a:count
  while cnt > 0

    " Look for the minimum indentation in the current visual region.
    let l = s:l0
    let idnt_invalid = 1000
    let idnt = idnt_invalid
    while l <= s:l1
      if !(getline(l) =~ "^\\s*$")
        let idnt = min([idnt, indent(l)])
      endif
      let l += 1
    endwhile

    " Keep track of where the range should be expanded to.
    let l_1 = s:l0
    let l_1o = l_1
    let l2 = s:l1
    let l2o = l2

    " If we are highlighting only blank lines, we may not have found a
    " valid indent. In this case we need to look for the next and previous
    " non blank lines and check which of those has the largest indent.
    if idnt == idnt_invalid
      let idnt = 0
      let pnb = prevnonblank(s:l0)
      if pnb
        let idnt = max([idnt, indent(pnb)])
        let l_1 = pnb
      endif
      let nnb = nextnonblank(s:l0)
      if nnb
        let idnt = max([idnt, indent(nnb)])
      endif

      " If we are in whitespace at the beginning of a block, skip over
      " it when we are selecting the range. Similarly, if we are in
      " whitespace at the end, ignore it.
      if idnt > indent(pnb)
        let l_1 = nnb
      endif
      if idnt > indent(nnb)
        let l2 = pnb
      endif
    endif

    " Search backward for the first line with less indent than the target
    " indent.
    let blnk = include_blanks && getline(l_1) =~ "^\\s*$"
    while l_1 > 0 && (blnk || indent(l_1) >= idnt)
      if g:indent_object_except_first_level && idnt == 0 && blnk
        break
      endif
      if !blnk || !a:inner
        let l_1o = l_1
      endif
      let l_1 -= 1
      let blnk = include_blanks && getline(l_1) =~ "^\\s*$"
    endwhile

    " Search forward for the first line with more indent than the target
    " indent.
    let line_cnt = line("$")
    let blnk = include_blanks && getline(l2) =~ "^\\s*$"
    while l2 <= line_cnt && (blnk || indent(l2) >= idnt)
      if g:indent_object_except_first_level && idnt == 0 && blnk
        break
      endif
      if !blnk || !a:inner
        let l2o = l2
      endif
      let l2 += 1
      let blnk = include_blanks && getline(l2) =~ "^\\s*$"
    endwhile

    " Determine which of these extensions to include. Include neither if
    " we are selecting an 'inner' object. Exclude the bottom unless are
    " told to include it.
    let idnt2 = max([indent(l_1), indent(l2)])
    if indent(l_1) < idnt2 || a:inner
      let l_1 = l_1o
    endif
    if indent(l2) < idnt2 || a:inner || !a:incbelow
      let l2 = l2o
    endif
    let l_1 = max([l_1, 1])
    let l2 = min([l2, line("$")])

    " Extend the columns to the start and end.
    " If inner is selected, set the final cursor pos to the start
    " of the text in the line.
    let c_1 = 1
    if a:inner
      let c_1 = match(getline(l_1), "\\c\\S") + 1
    endif
    let c2 = len(getline(l2))
    if !a:inner
      let c2 += 1
    endif

    " Make sure there's no change if we haven't really made a
    " significant change in linewise mode - this makes sure that
    " we can iteratively increase selection in linewise mode.
    if itr_cnt == 0 && vismode ==# 'V' && s:l0 == l_1 && s:l1 == l2
      let c_1 = s:c0
      let c2 = s:c1
    endif

    " Check whether the visual region has changed.
    let chg = 0
    let chg = chg || s:l0 != l_1
    let chg = chg || s:l1 != l2
    let chg = chg || s:c0 != c_1
    let chg = chg || s:c1 != c2

    if vismode ==# 'V' && new_vis
      let chg = 1
    endif

    " Update the vars.
    let s:l0 = l_1
    let s:l1 = l2
    let s:c0 = c_1
    let s:c1 = c2

    " If there was no change, then don't decrement the count (it didn't
    " count because it didn't do anything).
    if chg
      let cnt = cnt - 1
    else
      " Since this didn't work, push the selection back one char. This
      " will have the effect of getting the enclosing block. Do it at
      " the beginning rather than the end - the beginning is very likely
      " to be only one indentation level different.
      if s:l0 == 0
        return
      endif
      let s:l0 -= 1
      let s:c0 = len(getline(s:l0))
    endif

    let itr_cnt += 1

  endwhile

  " Apply the range we have found. Make sure to use the current visual mode.
  call cursor(s:l0, s:c0)
  exe "normal! " . vismode
  call cursor(s:l1, s:c1)
  normal! o

  " Update these static variables - we need to keep these up-to-date between
  " invocations because it's the only way we can detect whether it's a new
  " visual mode. We need to know if it's a new visual mode because otherwise
  " if there's a single line block in visual line mode and we select it with
  " "V", we can't tell whether it's already been selected using Vii.
  exe "normal! \<Esc>"
  let s:l0 = line("'<")
  let s:l1 = line("'>")
  let s:c0 = col("'<")
  let s:c1 = col("'>")
  normal! gv0o0

endfunction

function! HandleTextObjectMapping(inner, incbelow, vis, range)
  call TextObject(a:inner, a:incbelow, a:vis, a:range, v:count1)
endfunction
