" basic setup {{{
set nocompatible
set number
colorscheme darkblue
set guifont=consolas:h12
set foldmethod=indent
set foldlevel=99
set splitright
set guioptions-=r guioptions-=L
set wildmenu
set foldlevelstart=0
" }}}

" environment manipulation {{{
" change temp-file default directories so Vim has permission to write
let $TMP="c:/users/e018462/my documents/vim_temp"
let $TEMP="c:/users/e018462/my documents/vim_temp"
set directory=.,$TMP,$TEMP

" set PYSRC env variable, not sure I'll use this
let $PYSRC="c:/users/e018462/my documents/py_src"

" set SRC env variable, this is where I'll keep my git repos
let $SRC = "c:/users/e018462/documents/src"

" change working directory to $HOME
" cd $SRC 
" }}}

"auto-commands {{{
" turn off nowrap, relativenumber on on all buffers
augroup onAllBuffsOpen 
    autocmd!
    autocmd BufNewFile,BufRead * set nowrap rnu
augroup END

" configure python
augroup onPyBuffsOpen
    autocmd!
    autocmd BufNewFile,BufRead *.py
        \ set tabstop=4     |
        \ set softtabstop=4 |
        \ set shiftwidth=4  |
        \ set textwidth=79  |
        \ set expandtab     |
        \ set autoindent    |
        \ set nowrap        |
        \ :nnoremap $ mlviw"*yo?^\s*\(def\\|\)\s*\zs<esc>"*p$a\ze\s*\(=\\|(\)<esc>dd:@"<cr> |
        \ set fileformat=unix
augroup END

" configure rexx
augroup onRexxBuffsOpen
    autocmd!
    autocmd BufNewFile,BufRead *.rexx
        \ set tabstop=4     |
        \ set softtabstop=4 |
        \ set shiftwidth=4  |
        \ set textwidth=71  |
        \ set expandtab     |
        \ set autoindent    |
        \ set nowrap        |
augroup END

" configure cobol
augroup onCobolBuffsOpen
    autocmd!
    autocmd BufNewFile,BufRead *.cbl
        \ set filetype=cobol      |
        \ set nowrap              |
        \ let cobol_legacy_code=1 
    autocmd FileType cobol set sw=4 sts=4 et sta tw=72 
    "    \ set nowrap
    "    \ vertical resize 72   |
    "    \ syn match Concealed '^.\{6\}'   conceal |
    "    \ syn match Concealed '^\d\{6\}$' conceal |
    "    \ syn match Concealed '\d\{8\}$'  conceal |
    "    \ set conceallevel=2   |
    "    \ set concealcursor=nc |
augroup END

" configure vimscript
augroup onVimScriptBuffsOpen
    autocmd!
    autocmd filetype vim set foldmethod=marker
augroup END

" send enter key press to NERDTree (I think?)
augroup onNERDTreeEnterPress
    autocmd!
    autocmd VimEnter * NERDTree $SRC  
    autocmd VimEnter * wincmd p
    " autocmd BufEnter * lcd %:p:h
augroup END
"}}}

" I don't know {{{
source $VIMRUNTIME/vimrc_example.vim
source $VIMRUNTIME/mswin.vim
behave mswin
" }}}

" load plugins {{{
execute pathogen#infect()
"}}}

" custom key mappings {{{
"   tabs
        nnoremap <C-S-tab> :tabprevious<CR>
        nnoremap <C-tab>   :tabnext<CR>
        nnoremap <C-t>     :tabnew<CR>
        inoremap <C-S-tab> <Esc>:tabprevious<CR>i
        inoremap <C-tab>   <Esc>:tabnext<CR>i
        inoremap <C-t>     <Esc>:tabnew<CR> 
"   windows
        nmap  <silent> <A-k> :wincmd h<CR>
        nmap  <silent> <A-l> :wincmd l<CR>
        nnoremap <C-J> <C-W><C-J>
        nnoremap <C-K> <C-W><C-K>
        nnoremap <C-L> <C-W><C-L>
        nnoremap <C-H> <C-W><C-H>
"   buffers
        " map      <C-J> :bnext<CR>
        " map      <C-K> :bprev<CR>
"   CtrlP and NERDTree
        nmap     <C-p><C-p> :CtrlP $SRC<CR>
        nmap     <C-p><C-o> :CtrlPBuffer<CR>
        nmap     <C-p><C-i> :CtrlPMRU<CR>
        map      <C-\>      :NERDTreeToggle<CR>
"   mapping F-keys to similar commands in ISPF
        nnoremap <F2>    :tabnew<CR> 
        map      <S-F2>  :vnew<CR>
        nnoremap <F3>    :BD<cr>
        nnoremap <S-F3>  <C-w>q
        map      <F5>    :bprev<CR>
        map      <F6>    :bnext<CR>
        nnoremap <F7>    <C-u>
        nnoremap <S-F7>  gg
        nnoremap <S-F8>  G
        nnoremap <F8>    <C-d>
        nnoremap <F10>   zh
        nnoremap <F11>   zl
        nnoremap <S-F10> zH
        nnoremap <S-F11> zL
"   size vsplits equally 
 	nnoremap <leader>eq <c-w>=  	
"   misc
        nnoremap H 0
        nnoremap L $
        nnoremap <leader>svq :source $myvimrc<cr><c-w>q
        nnoremap <leader>sv :source $myvimrc<cr>
        nnoremap <leader>ev :vnew<cr>:e $myvimrc<cr>
        nnoremap <space> za
        inoremap jk <esc>
        inoremap JK <esc>
        inoremap jK <esc>
        inoremap <esc> <nop>
        vnoremap jk <esc>
        vnoremap JK <esc>
        vnoremap jK <esc>
        vnoremap <esc> <nop>
        nmap     ; kzt
        nmap     , jzt
        nnoremap <C-n> :call NumberToggle()<cr>
        "nmap    oo o<Esc>k
        "nmap    Oo O<Esc>j
        "nmap    OO O<Esc>j
"   clear trailing sequence numbers
    "   ë is equivalent to <A-k>
        nmap     ë       :%s/.\{72\}\zs.\{1,\}//gc<cr>

"   What is the current syntax highlighting group?
        map <F12> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">" . " FG:" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"fg#") . " BG:" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"bg#")<CR>
" }}}

" custom functions {{{
" configure VimDiff so it actually works, instead of throwing erros
set diffexpr=MyDiff()
function! MyDiff()
  let opt = '-a --binary '
  if &diffopt =~ 'icase' | let opt = opt . '-i ' | endif
  if &diffopt =~ 'iwhite' | let opt = opt . '-b ' | endif
  let arg1 = v:fname_in
  if arg1 =~ ' ' | let arg1 = '"' . arg1 . '"' | endif
  let arg2 = v:fname_new
  if arg2 =~ ' ' | let arg2 = '"' . arg2 . '"' | endif
  let arg3 = v:fname_out
  if arg3 =~ ' ' | let arg3 = '"' . arg3 . '"' | endif
  if $VIMRUNTIME =~ ' '
    if &sh =~ '\<cmd'
      if empty(&shellxquote)
        let l:shxq_sav = ''
        set shellxquote&
      endif
      let cmd = '"' . $VIMRUNTIME . '\diff"'
    else
      let cmd = substitute($VIMRUNTIME, ' ', '" ', '') . '\diff"'
    endif
  else
    let cmd = $VIMRUNTIME . '\diff'
  endif
  silent execute '!' . cmd . ' ' . opt . arg1 . ' ' . arg2 . ' > ' . arg3
  if exists('l:shxq_sav')
    let &shellxquote=l:shxq_sav
  endif
endfunction

" this function and the keymapping below are to toggle 
" relative line numbers
function! NumberToggle()
  if(&relativenumber == 1)
    set relativenumber!
  else
    set relativenumber
  endif
endfunc
" }}}
