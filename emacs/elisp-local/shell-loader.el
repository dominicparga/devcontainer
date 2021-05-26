;;; package --- Setup shell loader package
;;; Commentary:

;;; The shell loader package is designed to open all available shells
;;; on the right hand side of the current window. The shells will be
;;; vertically stacked.

;;; Code:

(defun ff/load-buffers (prefix)
  ;; filter all buffers that begin with the given prefix
  (setq filtered-buffers '())
  (dolist (name (mapcar #'buffer-name (buffer-list)))
    (if (string-match prefix name)
        (add-to-list 'filtered-buffers name)))
  ;; sort the buffers
  (cl-sort filtered-buffers 'string-lessp :key 'downcase)
  )

(defun ff/is-any-buffer-visible (buffer-list)
  (setq is-visible nil)
  (while (> (length buffer-list) 0)
    (setq next-buffer (pop buffer-list))
    (cond ((eq next-buffer (window-buffer (selected-window)))
           ;; Visible and focused
           (setq is-visible t))
          ((get-buffer-window next-buffer)
           ;; Visible and unfocused
           (setq is-visible t))))
  (eval is-visible))

(defun ff/open-windows (buffer-list)
  (split-window-right)
  (other-window 1)
  (while (> (length buffer-list) 1)
    ;; load the next buffer
    (setq next-buffer (pop buffer-list))
    (switch-to-buffer next-buffer)
    ;; and open a new window below the current one for the next buffer
    ;; in the list
    (split-window-below)
    (other-window 1)
    )
  ;; there is one buffer remaining and one last window to be filled
  (switch-to-buffer (pop buffer-list))
  (balance-windows)
  )

(defun ff/close-windows (buffer-list)
  (while (> (length buffer-list) 0)
    ;; switch to the next window and close it
    (setq next-window (get-buffer-window (pop buffer-list)))
    (delete-window next-window)
    )
  (balance-windows)
  )

(defun ff/toggle-windows-with-prefix (prefix start-cmd)
  (setq buffer-names (ff/load-buffers prefix))
  (cond ((= (length buffer-names) 0)
         ;; no buffers available -> create a new one
         (split-window-right)
         (other-window 1)
         ;; initiate whatever is given by the user
         (eval start-cmd)
         (balance-windows)
         )
        (t
         (if (ff/is-any-buffer-visible buffer-names)
             (ff/close-windows buffer-names)
           (ff/open-windows buffer-names)))))

(provide 'shell-loader)

;;; shell-loader.el ends here
