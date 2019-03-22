""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" {{{
"
" This script is made only for Neovim.
"
" Headers:
" Headings are organized like this:
"
" ==============================================================================
" | Heading                                                                  {{{
" ==============================================================================
"
" ------------------------------------------------------------------------------
" | subHeading                                                               {{{
" ------------------------------------------------------------------------------
"
" do something
"
"
"
" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -
" | SubSubHeading                                                            {{{
" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -
"
" do something
"
"
"                                                                            }}}
" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -
" | SubSubHeading                                                            {{{
" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -
"
" do something
"
"                                                                            }}}
"                                                                            }}}
" ------------------------------------------------------------------------------
" | subHeading                                                               {{{
" ------------------------------------------------------------------------------
"
" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -
" | SubSubHeading                                                            {{{
" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -
"
" do something
"
"
"                                                                            }}}
" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -
" | SubSubHeading                                                            {{{
" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -
"
" do something
"                                                                            }}}
"                                                                            }}}
"                                                                            }}}
" ==============================================================================
" | Heading                                                                  {{{
" ==============================================================================
"
" do something
"                                                                            }}}
"
"
"
" Markers:
" Lines, that only contain markers, are seen as empty lines.
"
" Empty Lines:
" There are three empty lines before every heading if there is text between it and
" the previous heading.
" There is one empty line between a heading and its content.
"
" There are no lines between a new heading and the previous line containing
" the ending marker of the previous heading. This leads in a better overview in
" collapsed state.
"
" Table Of Contents:
"
" TODO
"
"                                                                           }}}
"                                                made by Dominic Parga Cacheiro
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""



" ==============================================================================
" | load vim-plug and plugins                                                {{{
" ==============================================================================

" ------------------------------------------------------------------------------
" | install and init vim-plug                                                {{{
" ------------------------------------------------------------------------------

if empty(glob("~/.local/share/nvim/site/autoload/plug.vim"))
    execute "!curl -fLo ~/.local/share/nvim/site/autoload/plug.vim "
                \ . "--create-dirs "
                \ . "https://raw.githubusercontent.com"
                \ . "/junegunn/vim-plug/master/plug.vim"
endif

call plug#begin("~/.local/share/nvim/plugged")


"                                                                            }}}
" ------------------------------------------------------------------------------
" | colors                                                                   {{{
" ------------------------------------------------------------------------------

Plug 'arcticicestudio/nord-vim'

Plug 'iCyMind/NeoSolarized'
" high, low are also possible
let g:neosolarized_contrast = "normal"
" e.g. tabs; high, low are also possible
let g:neosolarized_visibility = "normal"
let g:neosolarized_vertSplitBgTrans = 1
let g:neosolarized_bold = 1
let g:neosolarized_underline = 1
let g:neosolarized_italic = 0

Plug 'dracula/vim'

Plug 'MaxSt/FlatColor'

Plug 'zacanger/angr.vim'


"                                                                            }}}
" ------------------------------------------------------------------------------
" | utility                                                                  {{{
" ------------------------------------------------------------------------------

" file tree
Plug 'scrooloose/nerdtree'
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'
let NERDTreeShowHidden = 1
let NERDTreeShowLineNumbers = 1

" quick and nice searching plugin using s
"Plug 'justinmk/vim-sneak'

" window swapping
Plug 'wesQ3/vim-windowswap'
let g:windowswap_map_keys = 1

" tab for autocompletion
"Plug 'ervandew/supertab'
"let g:SuperTabDefaultCompletionType = "context"
"let g:SuperTabContextTextOmniPrecedence = ['&omnifunc', '&completefunc']
"let g:SuperTabDefaultCompletionType = "<C-X><C-O>"
"let g:SuperTabLongestEnhanced = 1
"autocmd FileType * if &omnifunc != '' | call SuperTabChain(&omnifunc, "<c-p>") | endif
"set completeopt=menu,menuone,preview,noinsert
"autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

" use graphical version of vim's undo-tree
Plug 'sjl/gundo.vim'


"                                                                            }}}
" ------------------------------------------------------------------------------
" | generic programming support                                              {{{
" ------------------------------------------------------------------------------

" autoclosing, e.g. brackets
"Plug 'Townk/vim-autoclose'
Plug 'Raimondi/delimitMate'



" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -
" |  Language Server Protocol (LSP) (before deoplete)                           {{{
" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -

Plug 'autozimu/LanguageClient-neovim', {
\ 'branch': 'next',
\ 'do': 'bash install.sh',
\ }
" Required for operations modifying multiple buffers like rename.
set hidden

let g:LanguageClient_autoStart=1

let g:LanguageClient_serverCommands = {}
let g:LanguageClient_serverCommands.python = ['/Users/dominic/Library/Python/3.6/bin/pyls']


                                                                           " }}}
" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -
" | linting <=> static syntax analysis                                       {{{
" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -

" neomake: additional configuration after 'call plug#end()'
"Plug 'neomake/neomake'

"Plug 'w0rp/ale'
"let g:ale_sign_column_always = 0
"let g:ale_sign_error = '>>'
"let g:ale_sign_warning = '--'
"let g:ale_lint_on_text_changed = 0
"let g:ale_lint_on_enter = 0
"let g:ale_lint_on_save = 1
" open a window if ALE detect anything
"let g:ale_open_list = 1
"let g:ale_linters = {
"\ 'c': ['gcc'],
"\ 'cpp': ['g++'],
"\ 'java': ['javac']
"\}


"                                                                            }}}
" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -
" | analysis                                                                 {{{
" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1
let g:deoplete#auto_completion_start_length = 1
let g:deoplete#auto_complete_delay = 0

let g:deoplete#omni_patterns = {}
let g:deoplete#omni#input_patterns = {}

" after deoplete!
Plug 'wokalski/autocomplete-flow'
" if neosnippet is not enabled to prevent adding paren after func
"let g:autocomplete_flow#insert_paren_after_function = 0

" needed for func argument completion
Plug 'Shougo/neosnippet'
let g:neosnippet#enable_completed_snippet = 1

" needed for func argument completion
" default snippets
Plug 'Shougo/neosnippet-snippets'


"                                                                            }}}
" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -
" | completion sources                                                       {{{
" - - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -- - - - - - -

" vim commands
Plug 'Shougo/neco-vim'

" asynchronous C/C++/Objective-C/Objective-C++ completion for Neovim
Plug 'zchee/deoplete-clang'
let g:deoplete#sources#clang#libclang_path = "/opt/local/libexec/llvm-6.0/lib/libclang.dylib"
let g:deoplete#sources#clang#clang_header = "/opt/local/libexec/llvm-6.0/lib/clang/6.0.0/include"

" include completion framework for neocomplete/deoplete
" (e.g. completion from C++ header files)
Plug 'Shougo/neoinclude.vim'

" Java completion
Plug 'artur-shaik/vim-javacomplete2'

" python completion
Plug 'zchee/deoplete-jedi'
let g:deoplete#sources#jedi#statement_length = 50   " default: 50
let g:deoplete#sources#jedi#enable_cache = 1        " default: 1
let g:deoplete#sources#jedi#show_docstring = 1      " default: 0

" general
set omnifunc=syntaxcomplete#Complete

"                                                                            }}}
"                                                                            }}}
" ------------------------------------------------------------------------------
" | special programming support                                              {{{
" ------------------------------------------------------------------------------

" LaTeX
"Plug 'LaTeX-Box-Team/LaTeX-Box'
Plug 'lervag/vimtex'
" for empty tex files as default,
" so filetype is set to 'tex' instead of 'plaintex'
let g:tex_flavor = "latex"

" markdown
Plug 'iamcco/markdown-preview.vim'
" open preview window once entered the markdown buffer
let g:mkdp_auto_start = 1
" auto open preview window when you edit the markdown file
let g:mkdp_auto_open = 0
" auto close current preview window when leaving markdown buffer
let g:mkdp_auto_close = 1
" 1 <=> refresh md when saving buffer or leaving insert mode
" 0 <=> auto refresh markdown as you edit or move the cursor
let g:mkdp_refresh_slow = 0
" MarkdownPreview command can be use for all files
let g:mkdp_command_for_global = 0


"                                                                            }}}
" ------------------------------------------------------------------------------
" | git support                                                              {{{
" ------------------------------------------------------------------------------

" shows changes in files controlled by git
Plug 'airblade/vim-gitgutter'
" default was 4000 (ms)
set updatetime=250
" default value: 500
let g:gitgutter_max_signs = 500
let g:gitgutter_map_keys = 1
let g:gitgutter_highlight_lines = 0
" if laggy
"let g:gitgutter_realtime = 0
" if laggy
"let g:gitgutter_eager = 0


"                                                                            }}}
" ------------------------------------------------------------------------------
" | highlighting                                                             {{{
" ------------------------------------------------------------------------------

" a solid language pack
Plug 'sheerun/vim-polyglot'
let g:polyglot_disabled = ['latex']

" enhanced C++ highlighting
Plug 'octol/vim-cpp-enhanced-highlight'
let g:cpp_class_scope_highlight = 1
let g:cpp_member_variable_highlight = 1
" class name declaration
let g:cpp_class_decl_highlight = 1
" library concepts
let g:cpp_concepts_highlight = 1
" no user created functions
let g:cpp_no_function_highlight = 0
" curly brackets in corner brackets, e.g. [{1, 2}]
let c_no_curly_error = 1
" mostly correct but slow on large files
"let g:cpp_experimental_simple_template_highlight = 1
" faster than previous but incorrect corner cases
"let g:cpp_experimental_template_highlight = 1


"                                                                            }}}
" ------------------------------------------------------------------------------
" | theme / interface                                                        {{{
" ------------------------------------------------------------------------------

" improved status bar
Plug 'itchyny/lightline.vim'
" hide vim mode (e.g. INSERT, VISUAL)
set showmode
let g:lightline = {}
let g:lightline.colorscheme = 'nord'
let g:lightline.active = {
\ 'left': [ [ 'mode', 'paste' ],
\           [ 'readonly', 'filename', 'modified' ] ],
\ 'right': [ [ 'lineinfo' ],
\            [ 'percent' ],
\            [ 'fileformat', 'fileencoding', 'filetype' ] ] }
let g:lightline.inactive = {
\ 'left': [ [ 'filename' ] ],
\ 'right': [ [ 'lineinfo' ],
\            [ 'percent' ] ] }
let g:lightline.tabline = {
\ 'left': [ [ 'tabs' ] ],
\ 'right': [ [ 'close' ] ] }


"                                                                            }}}
" ------------------------------------------------------------------------------
" | finish vim-plug                                                          {{{
" ------------------------------------------------------------------------------
call plug#end()

" When writing a buffer.
"call neomake#configure#automake('w')
" When writing a buffer, and on normal mode changes (after 750ms).
"call neomake#configure#automake('w')
" When reading a buffer (after 1s), and when writing.
"call neomake#configure#automake('rw', 1000)

"                                                                            }}}
"                                                                            }}}
" ==============================================================================
" | editor settings                                                          {{{
" ==============================================================================

" ------------------------------------------------------------------------------
" | general                                                                  {{{
" ------------------------------------------------------------------------------

filetype plugin indent on
syntax on
" ignored in iTerm2
set guifont=Menlo:h14

" use correct colors from terminal
set termguicolors

set background=dark
colorscheme nord
set listchars=tab:>-,trail:_,extends:>,precedes:<
"set listchars=eol:¬,tab:>-,trail:_,extends:>,precedes:<
set list

" always display the status line
set laststatus=2
let mapleader = ","
let maplocalleader = ","

" http://tedlogan.com/techblog3.html
" number of visual spaces per tab
set tabstop=4
" number of spaces in tab when editing
set softtabstop=4
set shiftwidth=4
" replace tabs by spaces
set expandtab

" use clipboard with system clipboard
set clipboard=unnamed
" save history of changes
set undofile


"                                                                            }}}
" ------------------------------------------------------------------------------
" | completion window                                                        {{{
" ------------------------------------------------------------------------------

highlight Pmenu ctermfg=NONE ctermbg=236 cterm=bold
highlight PmenuSel ctermfg=NONE ctermbg=24 cterm=bold
highlight Pmenu guifg=NONE guibg=#64666d gui=bold
highlight PmenuSel guifg=NONE guibg=#204a87 gui=bold

set wildmode=longest:full,full


"                                                                            }}}
" ------------------------------------------------------------------------------
" | folding                                                                  {{{
" ------------------------------------------------------------------------------

set fillchars=vert:\|
set foldcolumn=2
highlight FoldColumn ctermfg=NONE ctermbg=NONE cterm=bold
highlight FoldColumn guifg=NONE guibg=NONE gui=bold


"                                                                            }}}
" ------------------------------------------------------------------------------
" | line numbers                                                             {{{
" ------------------------------------------------------------------------------

set number relativenumber
"augroup numbertoggle
"  autocmd!
"  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
"  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
"augroup END


"                                                                            }}}
" ------------------------------------------------------------------------------
" | cursor                                                                   {{{
" ------------------------------------------------------------------------------

" highlighting
if 0
    " enable highlighting of the current line
    set cursorline
    augroup cline
        au!
    "    au WinLeave,InsertEnter * set nocursorline
    "    au WinEnter,InsertLeave * set cursorline
        au WinLeave * set nocursorline
        au WinEnter * set cursorline
    augroup END
else
    " disable cursor highlighting
    set nocursorline
endif
" 999 <=> centered cursor
set scrolloff=12


"                                                                            }}}
" ------------------------------------------------------------------------------
" | wrapping                                                                 {{{
" ------------------------------------------------------------------------------

if 0
    " enable wrapping
    set wrap
    " activate smart indents in wrapped lines
    set breakindent
    " extra space for wrapped lines
    set breakindentopt=shift:2
    " wrap after any word
    set linebreak
else
    " disable wrapping
    set nowrap
    set nobreakindent
    set breakindentopt=
    set nolinebreak
endif


"                                                                            }}}
" ------------------------------------------------------------------------------
" | overlength highlighting                                                  {{{
" ------------------------------------------------------------------------------

set textwidth=80
" highlight three columns after 'textwidth'
set colorcolumn=+1

"hi colorcolumn ctermbg=lightgrey guibg=lightgrey
"highlight OverLength80 ctermbg=red ctermfg=white guibg=#592929
"match OverLength80 /\%>80v.\+/
"match OverLength100 /\%>100v.\+/


"                                                                            }}}
" ------------------------------------------------------------------------------
" | casing                                                                   {{{
" ------------------------------------------------------------------------------

set ignorecase
" searching 'the' is case-insensitive, searching 'The' is
set smartcase
" don't use first match when hitting tab first


"                                                                            }}}
" ------------------------------------------------------------------------------
" | searching                                                                {{{
" ------------------------------------------------------------------------------

" highlight found words
set hlsearch
" already search for and highlight written (but not yet finished) pattern
set incsearch
" fuzzy finding using :find
set path+=**

"                                                                            }}}
"                                                                            }}}
" ==============================================================================
" | utility                                                                  {{{
" ==============================================================================

augroup utility
    autocmd!
    " save if window is losing focus
    autocmd FocusLost * :wa
augroup END


" open this file (nvim/init.vim) quickly in quick view
nnoremap <Leader>ev :vsplit ${MYVIMRC}<CR>
" source this file (nvim/init.vim) quickly
nnoremap <Leader>sv :source ${MYVIMRC}<CR>
" use tab for cycling through completion menu
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
" closes top info window after completion is done
"autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif


" my name
iabbrev dpc Dominic Parga Cacheiro


" disable arrow movement, resize splits instead
nnoremap <Up>       <C-w>+
nnoremap <Down>     <C-w>-
nnoremap <Left>     <C-w><
nnoremap <Right>    <C-w>>

" add a line without entering insert mode
if 0
    nnoremap o o<Esc>
    nnoremap O O<Esc>
endif


" uppercase current selection and go to end of it
vnoremap <Leader>u U`>
" uppercase current word
nnoremap <Leader>u viwUe
inoremap <C-u> <Esc>viwUea


" DOUBLE QUOTES around visual selection and exit vmode
vnoremap <Leader>" <Esc>`>a"<Esc>`<i"<Esc>lv`>l<Esc>
" SINGLE QUOTES around visual selection and exit vmode
vnoremap <Leader>' <Esc>`>a'<Esc>`<i'<Esc>lv`>l<Esc>
" VERT BAR around visual selection and exit vmode
vnoremap <Leader><Bar> <Esc>`>a<Bar><Esc>`<i<Bar><Esc>lv`>l<Esc>
" (...) around visual selection and exit vmode
vnoremap <Leader>( <Esc>`>a)<Esc>`<i(<Esc>lv`>l<Esc>
vnoremap <Leader>) <Esc>`>a)<Esc>`<i(<Esc>lv`>l<Esc>
" [...] around visual selection and exit vmode
vnoremap <Leader>[ <Esc>`>a]<Esc>`<i[<Esc>lv`>l<Esc>
vnoremap <Leader>] <Esc>`>a]<Esc>`<i[<Esc>lv`>l<Esc>
" {...} around visual selection and exit vmode
vnoremap <Leader>{ <Esc>`>a}<Esc>`<i{<Esc>lv`>l<Esc>
vnoremap <Leader>} <Esc>`>a}<Esc>`<i{<Esc>lv`>l<Esc>


" get char and remove it if given pattern matches
" (used e.g. for eating space in abbreviations)
function! Eatchar(pat)
    let l:c = nr2char(getchar(0))
    return (l:c =~# a:pat) ? '' : l:c
endfunction


" toggle plugin sjl/gundo.vim
nnoremap <Leader>gu :GundoToggle<CR>


"                                                                            }}}
" ==============================================================================
" | sources                                                                  {{{
" ==============================================================================

" GENERAL TUTORIAL:
" learnvimscriptthehardway.stevelosh.com

" CUSTOM FOLD TEXT:
" stackoverflow.com/questions/27344118/setup-vim-to-cut-fold-line-after-80-characters

"                                                                            }}}

