;;; package --- Set up ansi-term
;;; Commentary:
;;; Sets up ansi-term config for Emacs

;;; Code:

(defun ff/start-vterm ()
  "Start Vterm terminal emulator."
  (interactive)
  (let* ((term-buffer-name "*vterm*")
         (start-cmd '(vterm)))
    (ff/start-term term-buffer-name start-cmd)
    ))


(defun ff/open-vterm (direction)
  "Opens new ansi-term either below or right of the current window."
  (if (equal (eval direction) "below")
      (split-window-below)
    (split-window-right))

  (balance-windows)
  (other-window 1)
  (vterm t)
  )

(defun ff/open-vterm-right ()
  (interactive)
  (ff/open-vterm "right")
)

(defun ff/open-vterm-below ()
  (interactive)
  (ff/open-vterm "below")
)

(defun ff/term-exec-hook ()
  "Delete the buffer once the terminal session is terminated."
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer-and-window)
          )))))

(defun vterm-counsel-yank-pop-action (orig-fun &rest args)
  (if (equal major-mode 'vterm-mode)
      (let ((inhibit-read-only t)
            (yank-undo-function (lambda (_start _end) (vterm-undo))))
        (cl-letf (((symbol-function 'insert-for-yank)
               (lambda (str) (vterm-send-string str t))))
            (apply orig-fun args)))
    (apply orig-fun args)))

(use-package vterm
  :ensure-system-package ((cmake . cmake)
                          (libtool . libtool-bin)
                          ("/bin/zsh" . zsh))
  :commands vterm
  :hook ((vterm-mode . ff/term-exec-hook))
  :config
  (setq vterm-shell "/bin/zsh")
  (setq vterm-max-scrollback 10000)
  (setq explicit-shell-file-name "/bin/zsh")

  (define-key vterm-mode-map (kbd "<C-backspace>")
    (lambda () (interactive) (vterm-send-key (kbd "C-w"))))

  (advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)

  :bind (("C-x j" . ff/start-vterm)
         :map vterm-mode-map
         ("C-y" . term-paste)
         ("C-x 2" . ff/open-vterm-below)
         ("C-x 3" . ff/open-vterm-right)
         )
  )

(provide 'setup-vterm)

;;; setup-vterm.el ends here
