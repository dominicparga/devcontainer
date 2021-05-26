;;; package --- Set up eshell
;;; Commentary:
;;; Sets up eshell config for Emacs

;;; Code:
(defun read-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun dw/get-current-package-version ()
  (interactive)
  (let ((package-json-file (concat (eshell/pwd) "/package.json")))
    (when (file-exists-p package-json-file)
      (let* ((package-json-contents (read-file package-json-file))
             (package-json (ignore-errors (json-parse-string package-json-contents))))
        (when package-json
          (ignore-errors (gethash "version" package-json)))))))

(defun dw/map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun dw/get-git-status-prompt ()
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
    (seq-uniq (seq-filter 'identity (mapcar 'dw/map-line-to-status-char status-lines)))))

(defun dw/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
      (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

;; This prompt function mostly replicates my custom zsh prompt setup
;; that is powered by github.com/denysdovhan/spaceship-prompt.
(defun dw/eshell-prompt ()
  (let ((current-branch (magit-get-current-branch))
        (package-version (dw/get-current-package-version)))
    (concat
     "\n"
     (propertize (system-name) 'face `(:foreground "#62aeed"))
     (propertize " ॐ " 'face `(:foreground "white"))
     (propertize (dw/get-prompt-path) 'face `(:foreground "#82cfd3"))
     (when current-branch
       (concat
        (propertize " • " 'face `(:foreground "white"))
        (propertize (concat " " current-branch) 'face `(:foreground "#c475f0"))))
     (when package-version
       (concat
        (propertize " @ " 'face `(:foreground "white"))
        (propertize package-version 'face `(:foreground "#e8a206"))))
     (propertize " • " 'face `(:foreground "white"))
     (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#5a5b7f"))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face `(:foreground "red2"))
       (propertize "\nλ" 'face `(:foreground "#aece4a")))
     (propertize " " 'face `(:foreground "white")))))

(defun ff/open-eshell-below ()
  "Opens new eshell either below of the current window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1)
  (eshell (ff/random-string 5))
  )

(defun dw/eshell-configure ()
  (use-package xterm-color)

  (push 'eshell-tramp eshell-modules-list)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output.
  (add-hook 'eshell-pre-command-hook
            '(lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            '(lambda () (setenv "TERM" "dumb")))

  ;; Use Ivy to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  (define-key eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (define-key eshell-mode-map (kbd "<home>") 'eshell-bol)

  ;; close eshell
  (define-key eshell-mode-map (kbd "C-d") '(lambda () (interactive) (kill-buffer-and-window) (balance-windows)))

  ;; if an eshell buffer is split, open a new session
  (define-key eshell-mode-map (kbd "C-x 2") 'ff/open-eshell-below)

  (setenv "PAGER" "cat")

  (setq eshell-prompt-function      'dw/eshell-prompt
        eshell-prompt-regexp        "^λ "
        eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions nil)
)

(defun ff/start-eshell ()
  "Start Vterm terminal emulator."
  (interactive)
  (ff/toggle-windows-with-prefix "*eshell" '(eshell t)))

(use-package eshell
  :hook (eshell-first-time-mode . dw/eshell-configure)
  :bind (("C-x e" . ff/start-eshell))
  :init
  (setq eshell-directory-name "~/.emacs.d/eshell/"))

(use-package eshell-z
  :hook ((eshell-mode . (lambda () (require 'eshell-z)))
         (eshell-z-change-dir .  (lambda () (eshell/pushd (eshell/pwd))))))

(provide 'setup-eshell)

;;; setup-eshell.el ends here
