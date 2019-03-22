let g:deoplete#omni_patterns.java = '[^. *\t]\.\w*'

setlocal omnifunc=javacomplete#Complete



" abbreviations ================================================================
" {{{

" std functions -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
" {{{

" add main method
" move cursor into it
" remove char, that was used for the abbrev
inoreabbrev <buffer> psvm 
    \public static void main(String[] args) {<CR>}
    \<Esc>O
    \<C-r>=Eatchar(getchar(0))<CR>

" add System.out.println("|");
" remove char, that was used for the abbrev
inoreabbrev <buffer> sysout 
    \System.out.println("");<Esc>2hi
    \<C-r>=Eatchar(getchar(0))<CR>

" add System.out.println("|");
" remove char, that was used for the abbrev
inoreabbrev <buffer> sout 
    \System.out.println("");<Esc>2hi
    \<C-r>=Eatchar(getchar(0))<CR>

" add System.out.println("|");
" remove char, that was used for the abbrev
inoreabbrev <buffer> syserr 
    \System.err.println("");<Esc>2hi
    \<C-r>=Eatchar(getchar(0))<CR>

" add System.out.println("|");
" remove char, that was used for the abbrev
inoreabbrev <buffer> serr 
    \System.err.println("");<Esc>2hi
    \<C-r>=Eatchar(getchar(0))<CR>

" }}}



" useful code body snippets -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
" {{{

" add if-clause
" move cursor into it
" remove char, that was used for the abbrev
inoreabbrev <buffer> if( 
    \if (<Esc>la
    \ {<CR>}
    \<Esc>ke2la
    \<C-r>=Eatchar(getchar(0))<CR>

" add else
" move cursor into it
" remove char, that was used for the abbrev
inoreabbrev <buffer> elif( 
    \else if (<Esc>la
    \ {<CR>}
    \<Esc>kfe2e2la
    \<C-r>=Eatchar(getchar(0))<CR>

" add empty for-loop
" move cursor into it
" remove char, that was used for the abbrev
inoreabbrev <buffer> for( 
    \for (<Esc>la
    \ {<CR>}
    \<Esc>ke2la
    \<C-r>=Eatchar(getchar(0))<CR>

" add for-loop using the attribute i
" move cursor into it
" remove char, that was used for the abbrev
inoreabbrev <buffer> fori 
    \for (int i = 0; i < ; i++) {<CR>}
    \<Esc>k$F;i
    \<C-r>=Eatchar(getchar(0))<CR>

" }}}

" }}}

