nnoremap <Leader>g :set operatorfunc=<SID>GrepOperator<CR>g@
vnoremap <Leader>g :<C-u>call <SID>GrepOperator(visualmode())<CR>

function! s:GrepOperator(type)
    " store yank register
    let l:reg_yank = @@

    " yank selection
    if a:type ==# 'v'
        normal! `<v`>y
    elseif a:type ==# 'char'
        normal! `[v`]y
    else
        return
    endif

    " execute grep search and open quickfix-window
    silent execute "grep! -R " . shellescape(@@) . " ."
    copen

    " restore old settings
    let @@ = l:reg_yank
endfunction

