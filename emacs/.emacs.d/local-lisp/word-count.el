(defun count-words-region (begin end)
  "Count the number of words in the region.

A word is whatever `forward-word' thinks is a word, except that `-'
and `_' are considered part of a word. 

If called interactively, displays result in minibuffer.  If called
from elisp, arguments BEGIN and END delimit the region, and the return
value is actual number of words."
  (interactive "r")
  (let ((org-table (syntax-table))
	(table (copy-syntax-table))
	(count 0))
    (save-excursion
      (goto-char begin)
      (unwind-protect 
	  (progn
	    (set-syntax-table table)
	    (modify-syntax-entry ?- "w")
	    (modify-syntax-entry ?_ "w")
	    (while (< (point) end)
	      (forward-word 1)
	      (setq count (1+ count))))
	(set-syntax-table org-table)))
    (if (interactive-p)
	(message "Region has %d words." count)
	count)))

(defun count-words-buffer ()
  "Count the number of words in the buffer.

A word is whatever `forward-word' thinks is a word, except that `-'
and `_' are considered part of a word. 

If called interactively, displays result in minibuffer.  If called
from elisp, return value is actual number of words."
  (interactive)
  (if (interactive-p)
      (message "Buffer has %d words."
	       (count-words-region (point-min) (point-max)))
      (count-words-region (point-min) (point-max))))

(provide 'word-count)