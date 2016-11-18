" plugins {{{
  " bufkill         - sane quit buffer commands
  " commentary      - simple comment-toggling
  " ctrlp           - fuzzy file finder
  " dbext           - relational database interface
  " easymotion      - easy, precise motions
  " minibufexpl     - simple buffer listing bar
  " moveIt_moveIt   - move lines of code easily
  " nerdtree        - intuitive file explorer
  " surround        - wrap text in various delimiters
  " todo.txt        - syntax highlighting for todo.txt files
    execute pathogen#infect()
"}}}

" basic setup {{{

  " general gui/application options
  " ===============================
    set nocompatible
    set backspace=indent,eol,start
    set guifont=consolas:h12
    set guioptions+=c           " use console dialogs, not popups
    set guioptions-=m           " disable menubar
    set guioptions-=T           " disable toolbar
    set guioptions-=r           " disable scrollbar right
    set guioptions-=L           " disable scrollbar left
    set splitright              " set vertical as default split
    set hidden                  " allow files with pending changes
                                "   to be unfocused without writing
        colorscheme darkblue
        syntax on
        filetype plugin indent on
        behave mswin

  " line formatting
  " ==============
    set number                  " show line numbers
    set relativenumber          " make line numbers relative to cursor
    set nowrap                  " turn off soft line wrapping

  " tab character/indentation
  " =========================
    set tabstop=4     
    set softtabstop=4 
    set shiftwidth=4  
    set textwidth=79  
    set expandtab     
    set autoindent   

  " wildmenu
  " ========
    set wildmenu
    set wildignore+=*.pyc                   " ignore python byte-code files
    set wildignore+=**/node_modules/**      " ignore node_modules
    set wildignore+=**\\node_modules\\**    " ...and on systems that use '\'
    set wildignore+=**/__pycache__/**       " ignore __pycache__
    set wildignore+=**\\__pycache__\\**     " ...and on systems that use '\'
    set wildignore+=*.swp                   " ignore Vim .swp files
    set wildignore+=*.*~                    " ignore backup/temp files

  " backup/swap files
  " =================
    set nobackup            
    set nowritebackup
    set noswapfile

  " regex searching
  " =========
    set incsearch
    set hlsearch

" }}}

" environment manipulation {{{

    let vim_env_rc = expand('~') . 'env_profile.vim'
    try 
        exe "source " . vim_env_rc
    catch
        echom vim_env_rc . " not found"
    endtry

" }}}

" dbext connection details {{{

    let vim_dbext_rc = expand('~') . 'dbext_profile.vim'
    try
        exe "source " . vim_dbext_rc
    catch
        echom vim_dbext_rc . " not found"
    endtry

" }}}

"auto-commands {{{

  " Turn off alarm bells
  " ====================
    if has("autocmd") && has("gui_running")
        augroup SetAlarmsOff
            autocmd!
            autocmd GUIEnter * set visualbell t_vb=
        augroup END
    endif

  " Configure text files
  " ====================
    augroup onTextBuffsOpen
        autocmd!
        autocmd BufNewFile,BufRead *.txt,*.md,*.markdown,*.log
            \ setlocal wrap
    augroup END

  " Configure python
  " ================
    augroup onPyBuffsOpen
        autocmd!
        autocmd BufNewFile,BufRead *.py
            \ setlocal tabstop=4         |
            \ setlocal softtabstop=4     |
            \ setlocal shiftwidth=4      |
            \ setlocal textwidth=79      |
            \ setlocal expandtab         |
            \ setlocal autoindent        |
            \ setlocal nowrap            |
            \ setlocal foldmethod=indent |
            \ setlocal foldlevel=99      |
            \ :nnoremap <buffer> $ :execute "normal! mlviw\"*y"<cr>:execute "normal!" .  '?^\s*\(def\\|\)\s*\zs' . @* . '\ze\s*\(=\\|(\)' . "\r"<cr> |
            \ setlocal fileformat=unix 
    augroup END
        " explanation of the nnoremap to $ in the above autocmd
        " for python buffers:
        "   ml          - set a marker at the cursor position 
        "                 and save to l
        "   viw         - select current word
        "   \"*y        - yank selected word to *
        "   ?^\s*       - look backwards for whitespace starting
        "                 a line 
        "   \(def\\|\)  - followed by optional literal 'def'
        "   \s*\zs      - followed by more whitespace, then begin 
        "                 selection 
        "   . @* .      - concatenate with the text in *
        "   \ze\s*      - end selection, followed by more whitespace
        "   \(=\\|(\)'  - followed by an '=' or '('

  " Configure rexx
  " ==============
    augroup onRexxBuffsOpen
        autocmd!
        autocmd BufNewFile,BufRead *.rexx
            \ setlocal tabstop=4     |
            \ setlocal softtabstop=4 |
            \ setlocal shiftwidth=4  |
            \ setlocal textwidth=71  |
            \ setlocal expandtab     |
            \ setlocal autoindent    |
            \ setlocal nowrap        
    augroup END

  " Configure cobol
  " ===============
    augroup onCobolBuffsOpen
        autocmd!
        autocmd BufNewFile,BufRead *.cbl
            \ setlocal filetype=cobol      |
            \ setlocal nowrap              |
            \ let cobol_legacy_code=1      |
            \ :nnoremap $ :execute "normal! mlviw\"*y"<cr>:execute "normal!" . '?^\(\d\{6\}\\|\s\{6\}\)\s*\(\d\d\\|\)\s*\zs' . @* . '\ze[ \.]' . "\r"<cr> 
        autocmd FileType cobol setlocal sw=4 sts=4 et sta tw=72 
    augroup END

  " Configure vimscript
  " ===================
    augroup onVimScriptBuffsOpen
        autocmd!
        autocmd filetype vim setlocal foldmethod=marker foldlevel=0
    augroup END

  " Configure NerdTree
  " ==================
    augroup onNERDTreeEnterPress
        autocmd!
        autocmd VimEnter * NERDTree $SRC
        autocmd VimEnter * wincmd p
        " autocmd BufEnter * lcd %:p:h
    augroup END
"}}}

" key mappings {{{

  " Screen tabs
  " ===========
    nnoremap <C-S-tab>  :tabprevious<CR>
    nnoremap <C-tab>    :tabnext<CR>
    nnoremap <C-t>      :tabnew<CR>
    inoremap <C-S-tab>  <Esc>:tabprevious<CR>i
    inoremap <C-tab>    <Esc>:tabnext<CR>i
    inoremap <C-t>      <Esc>:tabnew<CR> 

  " Screen windows
  " ==============
    noremap  <silent>   <A-K>   :wincmd h<CR>
    noremap  <silent>   <A-L>   :wincmd l<CR>
    noremap             <C-J>   <C-W><C-J>
    noremap             <C-K>   <C-W><C-K>
    noremap             <C-L>   <C-W><C-L>
    noremap             <C-H>   <C-W><C-H>
    noremap <leader>eq  <C-W>=  	            

  " Buffers
  " =======
    " map      <C-J> :bnext<CR>
    " map      <C-K> :bprev<CR>
    
  " CtrlP and NERDTree
  " ==================
    noremap <C-p><C-p> :exe "CtrlP $SRC"<CR>
    noremap <C-p><C-o> :CtrlPBuffer<CR>
    noremap <C-p><C-i> :CtrlPMRU<CR>
    noremap <C-\>      :NERDTreeToggle<CR>

  " ISPF-like key mappings
  " ======================
    noremap <F2>    :tabnew<CR> 
    noremap <S-F2>  :vnew<CR>
    noremap <F3>    :BD<CR>
    noremap <S-F3>  <C-w>q
    noremap <F5>    :bprev<CR>
    noremap <F6>    :bnext<CR>
    noremap <F7>    <C-u>
    noremap <S-F7>  gg
    noremap <S-F8>  G
    noremap <F8>    <C-d>
    noremap <F9>    :b#<cr>
    noremap <F10>   zh
    noremap <F11>   zl
    noremap <S-F10> zH
    noremap <S-F11> zL

  " Better motions
  " ==============
    noremap H 0
    noremap L $
    noremap ; kzt
    noremap , jzt
    noremap j gj
    noremap k gk
    noremap ` '
    noremap ' `

  " Escape key
  " ==========
    inoremap jk     <esc>
    inoremap JK     <esc>
    inoremap jK     <esc>
    inoremap <esc>  <nop>
    vnoremap jk     <esc>
    vnoremap JK     <esc>
    vnoremap jK     <esc>
    vnoremap <esc>  <nop>

  " Character/line editing
  " ======================
    nnoremap + mpO<esc>`p
    nnoremap = mpo<esc>`p
    nnoremap - mpjdd<esc>`p
    nnoremap _ mpkdd<esc>`p
    noremap vv V

  " File editing/sourcing
  " =====================
    noremap <leader>st  :source %<cr>
    noremap <leader>svq :source $myvimrc<cr><c-w>q
    noremap <leader>sv  :source $myvimrc<cr>
    noremap <leader>ev  :e $myvimrc<cr>
    noremap <leader>evw :vnew<cr>:e $myvimrc<cr>
    noremap <leader>qq  :call SwitchProject(proj_dir)<cr>
    noremap <leader>qw  :call SwitchProject(home_dir)<cr>

  " Miscellaneous
  " =============
    noremap <space> za
    noremap <leader>f   :set foldmethod=marker<cr>:set foldlevel=0<cr>
    noremap <leader>uf  :set foldlevel=99<cr>
    noremap <leader>dbp :call SetDB2EnvProd()<cr>
    noremap <leader>dbu :call SetDB2EnvUser()<cr>
    noremap <leader>dbt :call SetDB2EnvTest()<cr>
    noremap / /\c\v

    "   ë = <A-k>
    nnoremap     ë       :%s/.\{72\}\zs.\{1,\}//gc<cr>

    "   What is the current syntax highlighting group?
    nnoremap <F12> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">" . " FG:" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"fg#") . " BG:" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"bg#")<CR>
" }}}

" custom functions {{{

    " configure VimDiff so it actually works, instead of throwing errors
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

  " easily switch to various directories
  " in Vim, NERDTree and CtrlP
    function! SwitchProject(proj)
        let $SRC = a:proj
        cd $SRC
        NERDTree $SRC
        NERDTreeToggle
    endfunction

" }}}
