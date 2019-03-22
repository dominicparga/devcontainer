setlocal foldmethod=marker
setlocal foldlevel=0


" List Operations ==============================================================
" {{{

" In-Place Operations -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
" {{{

" List Operations - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
" {{{

function! Append(l, val)
" {{{
    call add(a:l, a:val)
    return a:l

endfunction " }}}


function! Remove(l, i)
" {{{
    call remove(a:l, a:i)
    return a:l

endfunction " }}}


function! Replace(l, i, val)
" {{{
    let a:l[a:i] = a:val
    return a:l

endfunction " }}}


function! Sort(l)
" {{{
    call sort(a:l)
    return a:l

endfunction " }}}


function! Reverse(l)
" {{{
    call reverse(a:l)
    return a:l

endfunction " }}}

" }}}



" Stack Operations - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
" {{{

function! Push(l, val)
" {{{
    call add(a:l, a:val)
    return a:l

endfunction " }}}


function! Pop(l, i)
" {{{
    call remove(a:l, a:i)
    return a:l

endfunction " }}}


function! Pop(l)
" {{{
    call remove(a:l, -1)
    return a:l

endfunction " }}}

" }}}

" }}}



" Out-Of-Place Operations -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
" {{{

" List Operations - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
" {{{

function! Appended(l, val)
" {{{
    let l:new_list = deepcopy(a:l)
    call add(l:new_list, a:val)
    return l:new_list

endfunction " }}}


function! Removed(l, i)
" {{{
    let l:new_list = deepcopy(a:l)
    call remove(l:new_list, a:i)
    return l:new_list

endfunction " }}}


function! Replaced(l, i, val)
" {{{
    let l:new_list = deepcopy(a:l)
    l:new_list[a:i] = a:val
    return l:new_list

endfunction " }}}


function! Sorted(l)
" {{{
    let l:new_list = deepcopy(a:l)
    call sort(l:new_list)
    return l:new_list

endfunction " }}}


function! Reversed(l)
" {{{
    let l:new_list = deepcopy(a:l)
    call reverse(l:new_list)
    return l:new_list

endfunction " }}}

" }}}



" Stack Operations - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
" {{{

function! Pushed(l, val)
" {{{
    let l:new_list = deepcopy(a:l)
    call add(l:new_list, a:val)
    return l:new_list

endfunction " }}}


function! Popped(l, i)
" {{{
    let l:new_list = deepcopy(a:l)
    call remove(l:new_list, a:i)
    return l:new_list

endfunction " }}}


function! Popped(l)
" {{{
    let l:new_list = deepcopy(a:l)
    call remove(l:new_list, -1)
    return l:new_list

endfunction " }}}

" }}}



" Higher-Order Functions - - - - - - - - - - - - - - - - - - - - - - - - - - - -
" {{{

function! Mapped(l, fn)
" {{{
    let l:new_list = deepcopy(a:l)
    call map(l:new_list, string(a:fn) . '(v:val)')
    return l:new_list

endfunction " }}}


function! Filtered(l, fn)
" {{{
    let l:new_list = deepcopy(a:l)
    call filter(l:new_list, string(a:fn) . '(v:val)')
    return l:new_list

endfunction " }}}


function! Removed(l, fn)
" {{{
    let l:new_list = deepcopy(a:l)
    call filter(l:new_list, '!' . string(a:fn) . '(v:val)')
    return l:new_list

endfunction " }}}

" }}}

" }}}

" }}}

