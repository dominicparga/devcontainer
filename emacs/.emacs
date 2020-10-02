;;; .emacs --- this file starts here

;;; Commentary:

;;; Code:

;; ==================================================================
;; Load Packages for different modes
;; ===================================================================

(setq user-full-name "Fabian Franzelin")
(setq user-mail-address "fabian.franzelin@de.bosch.com")
(setq inhibit-startup-echo-area-message "franzef")

;; Package Management
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(defvar local-load-path (expand-file-name "~/.emacs.d/local-lisp"))
(add-to-list 'load-path local-load-path)

;; Initialise packages
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install "use-package"))
(require 'use-package)

;; ===================================================================
;; Basic Settings
;; ===================================================================

;disable backup
(setq backup-inhibited t)
(setq make-backup-files nil)

;disable auto save
(setq auto-save-default nil)

;; disable scrollbar
(scroll-bar-mode -1)

;; disable menu
(menu-bar-mode nil)

;; enable revert from disk
(global-auto-revert-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(server-start)

;; Support Wheel Mouse Scrolling
(mouse-wheel-mode t)

;; Kill this buffer, instead of prompting for which one to kill
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; dir-local variables will be applied to remote files.
(setq enable-remote-dir-locals t)

; Turn on syntax colouring in all modes supporting it:
(global-font-lock-mode t)

; I want the name of the file I'm editing to be displayed in the
; title-bar.
(setq frame-title-format "%b")
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; (set-default-font "Monospace-11")
;; (set-frame-width (selected-frame) 100)
;; (set-frame-height (selected-frame) 100)

;; set encoding
;; set up unicode
(defvar prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'latin-1)
(setq buffer-file-coding-system 'utf-8)

;; no splash screen
(setq inhibit-splash-screen t)

;; no backup files
(setq make-backup-files nil)

; shift-pageUp/pageDown scrolls other window
(global-set-key (quote [S-prior]) (quote scroll-other-window-down))
(global-set-key (quote [S-next]) (quote scroll-other-window))

;; Color theme
(use-package material-theme
  :ensure t
  )

;; Set up recent files so I can get a list if them when I start
(recentf-mode 1)
(defvar recentf-max-saved-items 1200)

;; make text mode the default major mode and start auto-fill mode
;; auto auto-magically
(setq major-mode 'text-mode)

;; remove toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Syntax-Highlighting
(global-font-lock-mode t)

;; Key bindings
(global-set-key "\C-n" 'make-frame)
(global-set-key "\C-o" 'other-window)
(global-set-key "\M-f" 'select-frame-by-name)

;; F6 stores a position in a file F7 brings you back to this position
(global-set-key [f6] '(lambda () (interactive) (point-to-register ?1)))
(global-set-key [f7] '(lambda () (interactive) (register-to-point ?1)))

;; Show major mode
(global-set-key "\C-h\C-m" '(lambda() (interactive) (message "%s" major-mode)))

;; higlight the marked region (C-SPC) and use commands (like
;; latex-environment) on current region.
(transient-mark-mode t)

;; Indentation
(setq-default indent-tabs-mode nil)    ; use only spaces and no tabs
(setq tab-width 4)

;; Save history during sessions
(savehist-mode t)

;; Delete trailing white spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; It reverts when a dired buffer is reselected dired mode
(use-package dired
  :config (progn
            (setq dired-auto-revert-buffer t ; Auto update when buffer is revisited
                  dired-dwim-target t
                  dired-recursive-deletes 'always
                  dired-recursive-copies 'always
                  delete-by-moving-to-trash t
                  dired-listing-switches "-alh") ; human readable file sizes
            (add-hook 'dired-mode-hook 'auto-revert-mode)
            ))

;; Quick filter dired view
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map ("/" . dired-narrow)))

;; Let kill operate on the whole line when no region is selected
(use-package whole-line-or-region
  :ensure t
  :config (whole-line-or-region-global-mode))

;; Don't ask when symlinks in VCS are encountered, just edit the link, don't care about target location
(setq vc-follow-symlinks nil)

;; Remove indicators from the mode line
(use-package diminish
  :ensure t)

(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line))) ;; Moves point to first non-whitespace first, beginning of line then

;; Helps to keep track of your cursor
(use-package beacon
  :ensure t
  :config (progn
            (beacon-mode t)
            (setq beacon-color "#ff0000")))

;; volatile highlights - temporarily highlight changes from pasting etc
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; convenient setting to move between open buffers
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)

;; winner mode for for redo/undo window configurations
(winner-mode 1)

;; -------------------------------------------------------------------
;; Copy & Paste
;; -------------------------------------------------------------------

;; (transient-mark-mode 1) ; Now on by default: makes the region act
;; quite like the text "highlight" in many apps.  (setq
;; shift-select-mode t) ; Now on by default: allows shifted
;; cursor-keys to control the region.
(setq mouse-drag-copy-region nil) ;; stops selection with a mouse
                                  ;; being immediately injected to the
                                  ;; kill ring
(setq select-enable-primary nil)  ;; stops killing/yanking
                                  ;; interacting with primary X11
                                  ;; selection
(setq select-enable-clipboard t)  ;; makes killing/yanking interact with clipboard X11 selection

;; when pasting with middle click in Linux X11, paste at cursor position, not at click position
(setq mouse-yank-at-point t)

;; these will probably be already set to these values, leave them that
;; way if so!  (setf interprogram-cut-function 'x-select-text) (setf
;; interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; You need an emacs with bug #902 fixed for this to work properly. It
;; has now been fixed in CVS HEAD.  it makes "highlight/middlebutton"
;; style (X11 primary selection based) copy-paste work as expected if
;; you're used to other modern apps (that is to say, the mere act of
;; highlighting doesn't overwrite the clipboard or alter the kill
;; ring, but you can paste in merely highlighted text with the mouse
;; if you want to)
(setq select-active-regions t) ;  active region sets primary X11 selection
(global-set-key [mouse-2] 'mouse-yank-primary)  ; make mouse
                                                ; middle-click only
                                                ; paste from primary
                                                ; X11 selection, not
                                                ; clipboard and kill
                                                ; ring.

;; with this, doing an M-y will also affect the X11 clipboard, making
;; emacs act as a sort of clipboard history, at least of text you've
;; pasted into it in the first place.  (setq yank-pop-change-selection
;; t) ; makes rotating the kill ring change the X11 clipboard.

;; -------------------------------------------------------------------
;; Copy filename of buffer into clipboard
;; -------------------------------------------------------------------

(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(global-set-key [f12] 'my-put-file-name-on-clipboard)

;; -------------------------------------------------------------------
;; Auto Completion
;; -------------------------------------------------------------------
(use-package auto-complete
  :defer t
  :ensure t)

;; ;; dirty fix for having AC everywhere
;; (define-globalized-minor-mode real-global-auto-complete-mode
;;   auto-complete-mode (lambda ()
;;                        (if (not (minibufferp (current-buffer)))
;;                          (auto-complete-mode 1))
;;                        ))

;; (real-global-auto-complete-mode t)
;; (ac-flyspell-workaround)

;; -------------------------------------------------------------------
;; Fill Column Indicator
;; -------------------------------------------------------------------
(use-package fill-column-indicator
  :defer t
  :ensure t
  :init (progn
            (setq fci-rule-color "#f8f8f8") ;;#cccccc
            (define-globalized-minor-mode
              global-fci-mode fci-mode (lambda () (fci-mode 1)))
            )
  )

;; ;; Highlight character at "fill-column" position.
;; (use-package column-marker
;;   :ensure t
;;   :config (progn
;;             (set-face-background 'column-marker-1 "red")
;;             (setq truncate-lines 80)

;;             (add-hook 'python-mode-hook
;;                       (lambda () (interactive)
;;                         (column-marker-1 fill-column)))
;;             (add-hook 'ess-mode-hook
;;                       (lambda () (interactive)
;;                         (column-marker-1 fill-column)))
;;             )
;;   )

;; -------------------------------------------------------------------
;; Show total line number in the modline
;; -------------------------------------------------------------------
(defvar my-mode-line-buffer-line-count nil)
(make-variable-buffer-local 'my-mode-line-buffer-line-count)

(setq-default mode-line-format
              '("  " mode-line-modified
                (list 'line-number-mode "  ")
                (:eval (when line-number-mode
                         (let ((str "L%l"))
                           (when (and (not (buffer-modified-p)) my-mode-line-buffer-line-count)
                             (setq str (concat str "/" my-mode-line-buffer-line-count)))
                           str)))
                "  %p"
                (list 'column-number-mode "  C%c")
                "  " mode-line-buffer-identification
                "  " mode-line-modes))

(defun my-mode-line-count-lines ()
  (setq my-mode-line-buffer-line-count (int-to-string (count-lines (point-min) (point-max)))))

(add-hook 'find-file-hook 'my-mode-line-count-lines)
(add-hook 'after-save-hook 'my-mode-line-count-lines)
(add-hook 'after-revert-hook 'my-mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'my-mode-line-count-lines)

;; -------------------------------------------------------------------
;; Insert Pairs of Matching Elements
;; -------------------------------------------------------------------
(use-package autopair
  :defer t
  :ensure t
  :init (progn
          (autopair-global-mode t)
          )
  :config (progn
            (add-hook 'lisp-mode-hook #'(lambda () (setq autopair-dont-activate t)))

            ;; Add single and triple quote to the autopair list
            (add-hook 'python-mode-hook
                      #'(lambda ()
                          ;; (push '(?` . ?`)
                          ;;       (getf autopair-dont-pair :never))
                          (setq autopair-handle-action-fns
                                (list #'autopair-default-handle-action
                                      #'autopair-python-triple-quote-action))))

            ;; Some useful handler
            (add-hook 'TeX-mode-hook
                      #'(lambda ()
                          (set (make-local-variable 'autopair-handle-action-fns)
                               (list #'autopair-default-handle-action
                                     #'autopair-latex-mode-paired-delimiter-action))))

            ;; Latex Math mode pairs
            (add-hook 'TeX-mode-hook
                      #'(lambda ()
                          (modify-syntax-entry ?$ "\"")
                          (autopair-mode)))

            )
  )

;; Highlight parens
(show-paren-mode)
(setq show-paren-style 'mixed)	;; The entire expression

;; -------------------------------------------------------------------
;; Save history during sessions
;; -------------------------------------------------------------------
(use-package savehist
  :config (progn
            (savehist-mode t)
            (setq savehist-additional-variables '(extended-command-history kill-ring))))
                                        ; history-length 1000
                                        ; history-delete-duplicates

;; -------------------------------------------------------------------
;; Helm project
;; -------------------------------------------------------------------
(use-package helm
  :defer t
  :ensure t
  :config (progn
            (when (executable-find "curl")
              (setq helm-google-suggest-use-curl-p t))

            (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
                  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
                  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
                  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
                  helm-ff-file-name-history-use-recentf t
                  helm-echo-input-in-header-line t)

            (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
            (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
            (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
            )
  :bind (
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-c p s a" . helm-projectile-ack)
        )
  )

(use-package helm-ag
  :defer t
  :ensure t)


;; -------------------------------------------------------------------
;; ivy mode
;; -------------------------------------------------------------------
(use-package counsel
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d / %d) "
        ivy-height 25
        counsel-find-file-ignore-regexp (concat
                                         ;; File names beginning with # or .
                                         "\\(?:\\`[#.]\\)"
                                         ;; File names ending with # or ~
                                         "\\|\\(?:\\`.+?[#~]\\'\\)")
        ;; ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        )
  (ivy-mode t)
  (counsel-mode t)
  :bind* (
          ("C-s" . swiper)
          ("C-c C-r" . ivy-resume)
          ("C-." . counsel-imenu)
          ("C-c c" . counsel-org-capture)
          ))

;; Some additional hydras for ivy
(use-package ivy-hydra
  :ensure t)


;; Needed for ivy occur edit mode
(use-package wgrep
  :ensure t)

;; -------------------------------------------------------------------
;; projectile mode
;; -------------------------------------------------------------------

(use-package projectile
  :defer t
  :ensure t
  :init (progn
          (setq projectile-file-exists-remote-cache-expire nil)
          (setq projectile-mode-line '(:eval (format " Projectile[%s]" (projectile-project-name))))
          (setq projectile-globally-ignored-directories
                (quote
                 (".idea" ".eunit" ".git" ".hg" ".svn"
                  ".fslckout" ".bzr" "_darcs" ".tox"
                  "build" "target" "_build" ".history"
                  "tmp")))
          (setq projectile-require-project-root nil)
          ;; (setq projectile-indexing-method 'alien)
          ;; (setq projectile-enable-caching nil)
          (setq projectile-completion-system 'default)
          (setq projectile-svn-command "find . -type f -not -iwholename '*.svn/*' -print0")
          )
  :config (progn
            (projectile-mode 1)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
            )
  )

(use-package helm-projectile
  :defer t
  :ensure t
  :init (progn
          (setq projectile-completion-system 'helm)
          (helm-projectile-on)
          )
  )

;; -------------------------------------------------------------------
;; neotree
;; -------------------------------------------------------------------
(use-package neotree
  :init (progn
         (global-set-key [f8] 'neotree-toggle)
         )
  :config (progn
           (setq neo-smart-open t)
           (setq neo-autorefresh t)
           (setq neo-window-fixed-size nil)
          )
  :ensure t
  )

;; Set the neo-window-width to the current width of the
;; neotree window, to trick neotree into resetting the
;; width back to the actual window width.
;; Fixes: https://github.com/jaypei/emacs-neotree/issues/262
(eval-after-load "neotree"
  '(add-to-list 'window-size-change-functions
                (lambda (frame)
                  (let ((neo-window (neo-global--get-window)))
                    (unless (null neo-window)
                      (setq neo-window-width (window-width neo-window)))))))

;; -------------------------------------------------------------------
;; highlight symbol and replace
;; -------------------------------------------------------------------
;; (use-package hl-anything
;;   :ensure t)

(use-package highlight-symbol
  :defer t
  :ensure t
  :bind* (
         ("C-<f3>" . highlight-symbol)
         ("<f3>" . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-query-replace)
         )
  )

;; ===================================================================
;; Adjusting different modes
;; ===================================================================
;; -------------------------------------------------------------------
;; Ansi term for zsh in emacs buffer
;; -------------------------------------------------------------------
(defun oleh-term-exec-hook ()
  "Delete the buffer once the terminal session is terminated."
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'oleh-term-exec-hook)

(use-package multi-term
  :ensure t
  :config (progn
            (setq multi-term-program "/bin/zsh")
            (setq explicit-shell-file-name "/bin/zsh")
            )
  )

(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-y") 'term-paste))

(use-package helm-mt
  :after multi-term
  :ensure t
)

;; -------------------------------------------------------------------
;; C++
;; -------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :config (progn
            (yas-global-mode 1)
            )
  )

(use-package cmake-mode
  :mode (("\\.cmake$" . cmake-mode)
         ("CMakeLists.txt" . cmake-mode))
  :ensure t)

(use-package flycheck-clang-tidy
  :ensure t
  :after flycheck
  :config (progn
            (setq flycheck-clang-tidy-executable "/usr/bin/clang-tidy-athena-1")
            )
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup)
  )

(use-package c++-mode
  :mode (("\\.cpp\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode))
  :init (progn
          (add-hook 'c++-mode-hook (lambda ()
                                     (subword-mode t))))  ; CamelCase are two words
  )

;; (use-package company
;;   :ensure t
;;   :config (progn
;;             (setq company-idle-delay 0)
;;             )
;;   :bind* (
;;           ("M-/" . company-complete-common-or-cycle)
;;           )
;;   :hook (after-init-hook . global-company-mode)
;;   )

;; (use-package irony
;;   :ensure t
;;   :config
;;   (progn
;;     ;; If irony server was never installed, install it.
;;     (unless (irony--find-server-executable) (call-interactively #'irony-install-server))

;;     (add-hook 'c++-mode-hook 'irony-mode)
;;     (add-hook 'c-mode-hook 'irony-mode)

;;     ;; Use compilation database first, clang_complete as fallback.
;;     (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
;;                                                     irony-cdb-clang-complete))

;;     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;     ))

;; ;; I use irony with company to get code completion.
;; (use-package company-irony
;;   :ensure t
;;   :requires company irony
;;   :config
;;   (progn
;;     (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))))

;; ;; I use irony with flycheck to get real-time syntax checking.
;; (use-package flycheck-irony
;;   :ensure t
;;   :requires flycheck irony
;;   :config
;;   (progn
;;     (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

;; ;; Eldoc shows argument list of the function you are currently writing in the echo area.
;; (use-package irony-eldoc
;;   :ensure t
;;   :requires eldoc irony
;;   :config
;;   (progn
;;     (add-hook 'irony-mode-hook #'irony-eldoc)))


;; (use-package ag
;;   :ensure t)


;; (use-package c++-mode
;;   :mode (("\\.cpp\\'" . c++-mode)
;;          ("\\.hpp\\'" . c++-mode))
;;   :init (progn
;;           (add-hook 'c++-mode-hook (lambda ()
;;                                      (subword-mode t))))  ; CamelCase are two words
;;   )

;; (add-hook 'c-mode-hook (lambda ()
;;                          (subword-mode t))) ; CamelCase are two words

;; (setq c-basic-offset 2)

;; (c-add-style "my-cc-style"
;;              '("bsd" (c-offsets-alist . (
;;                                          (innamespace . 0)
;;                                          (namespace-open . 0)
;;                                          (namespace-close . 0)
;;                                          (cpp-macro . 0) ; indent macros like the surrounding code
;;                                          ))))
;; (setq c-default-style "my-cc-style")

;; ;; Find definition based on regexp
;; (use-package dumb-jump
;;   :ensure t)

;; ;; Proxy for different find-defintion engines
;; (use-package smart-jump
;;   :ensure t
;;   :config (progn
;;             (smart-jump-register :modes '(c-mode c++-mode)
;;                                  :jump-fn 'rtags-find-symbol-at-point
;;                                  :pop-fn 'rtags-location-stack-back
;;                                  :refs-fn 'rtags-find-all-references-at-point
;;                                  :should-jump (lambda ()
;;                                                 (and
;;                                                  (fboundp 'rtags-executable-find)
;;                                                  (rtags-executable-find "rc")
;;                                                  (rtags-is-indexed)))
;;                                  :heuristic 'point
;;                                  :async 500
;;                                  :order 1)
;;             (smart-jump-setup-default-registers)))

;; (use-package rtags
;;   :ensure t
;;   :config
;;   (progn
;;     (unless (rtags-executable-find "rc") (error "Binary rc is not installed!"))
;;     (unless (rtags-executable-find "rdm") (error "Binary rdm is not installed!"))

;;     (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
;;     (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
;;     (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
;;     (rtags-enable-standard-keybindings)

;;     (setq rtags-use-helm t)

;;     ;; Shutdown rdm when leaving emacs.
;;     (add-hook 'kill-emacs-hook 'rtags-quit-rdm)
;;     ))

;; ;; TODO: Has no coloring! How can I get coloring?
;; (use-package helm-rtags
;;   :ensure t
;;   :requires helm rtags
;;   :config
;;   (progn
;;     (setq rtags-display-result-backend 'helm)
;;     ))

;; ;; Use rtags for auto-completion.
;; (use-package company-rtags
;;   :ensure t
;;   :requires company rtags
;;   :config
;;   (progn
;;     (setq rtags-autostart-diagnostics t)
;;     (rtags-diagnostics)
;;     (setq rtags-completions-enabled t)
;;     (push 'company-rtags company-backends)
;;     ))

;; ;; Live code checking.
;; (use-package flycheck-rtags
;;   :ensure t
;;   :requires flycheck rtags
;;   :config
;;   (progn
;;     ;; ensure that we use only rtags checking
;;     ;; https://github.com/Andersbakken/rtags#optional-1
;;     (defun setup-flycheck-rtags ()
;;       (flycheck-select-checker 'rtags)
;;       (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;       (setq-local flycheck-check-syntax-automatically nil)
;;       (rtags-set-periodic-reparse-timeout 2.0)  ;; Run flycheck 2 seconds after being idle.
;;       )
;;     (add-hook 'c-mode-hook #'setup-flycheck-rtags)
;;     (add-hook 'c++-mode-hook #'setup-flycheck-rtags)
;;     ))

;; -------------------------------------------------------------------
;; CRAN R
;; -------------------------------------------------------------------
(use-package ess
  :defer t
  :ensure t
  :init (progn
          (setq ess-use-eldoc nil)
          ;; ESS will not print the evaluated commands, also speeds up the evaluation
          (setq ess-eval-visibly nil)
          ;; if you don't want to be prompted each time you start an interactive R session
          (setq ess-ask-for-ess-directory nil)
          )
  )

 ;;; ESS
(add-hook 'ess-mode-hook
          (lambda ()
            (ess-set-style 'C++ 'quiet)
            ;; Because
            ;;                                 DEF GNU BSD K&R  C++
            ;; ess-indent-level                  2   2   8   5  4
            ;; ess-continued-statement-offset    2   2   8   5  4
            ;; ess-brace-offset                  0   0  -8  -5 -4
            ;; ess-arg-function-offset           2   4   0   0  0
            ;; ess-expression-offset             4   2   8   5  4
            ;; ess-else-offset                   0   0   0   0  0
            ;; ess-close-brace-offset            0   0   0   0  0
            (add-hook 'local-write-file-hooks
                      (lambda ()
                        (ess-nuke-trailing-whitespace)))))
;; (setq ess-nuke-trailing-whitespace-p 'ask)
;; or even
(setq ess-nuke-trailing-whitespace-p t)

(setq tab-always-indent t)

;; -------------------------------------------------------------------
;; AUCTeX
;; -------------------------------------------------------------------
(use-package auctex
  :defer t
  :ensure t
  :init (progn
         (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
         (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
         ))

(use-package reftex
  :defer t
  :ensure t
  :init (progn
          (setq reftex-enable-partial-scans t)
          (setq reftex-use-multiple-selection-buffers t)
          (setq reftex-plug-into-AUCTeX t)
          (setq reftex-save-parse-info t)
          (setq reftex-use-external-file-finders t)
          (setq reftex-external-file-finders
               '(("tex" . "kpsewhich -format=.tex %f")
                 ("bib" . "kpsewhich -format=.bib %f")))
          (setq TeX-auto-save t)
          (setq TeX-auto-parse t)

          (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
          (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
          (add-hook 'LaTeX-mode-hook 'reftex-mode)      ; with Emacs latex mode
          ;; (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
          (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
          (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)     ; turn on pdf mode

          (setq LaTeX-command "latex -synctex=1")

          (setq reftex-default-bibliography (quote ("/home/franzefn/Promotion/Literatur/bibliothek/jabref/library.bib")))
          )
  :config (progn
            ;; Set index on document
            (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
            (add-hook 'reftex-mode-hook 'imenu-add-menubar-index)
            )
  )

;; emacs RefTeX
;; (setq reftex-ref-macro-prompt nil) ; skips picking the reference style

(eval-after-load 'reftex-vars
  '(progn
     ;; (also some other reftex-related customizations)
     (setq reftex-cite-format
           '((?\C-m . "\\cite[]{%l}")
             (?a . "\\citeauthor[]{%l}") ;; Franzelin
             (?f . "\\footcite[][]{%l}") ;; \footnote{[FR05]}
             (?t . "\\textcite[]{%l}")   ;; Franzelin [FR05] (without link)
             (?o . "\\citet[]{%l}")      ;; Franzelin [FR05] (with link)
             (?p . "\\parencite[]{%l}")  ;; [Franzelin, 2015] (without link)
             (?o . "\\citep[]{%l}")      ;; [Franzelin, 2015] (with link)
             (?n . "\\nocite{%l}")))
     )
  )

(eval-after-load
    "latex"
  '(TeX-add-style-hook
    "cleveref"
    (lambda ()
      (if (boundp 'reftex-ref-style-alist)
          (add-to-list
           'reftex-ref-style-alist
           '("Cleveref" "cleveref"
             (("\\cref" ?c) ("\\Cref" ?C) ("\\cpageref" ?d) ("\\Cpageref" ?D)))))
      (reftex-ref-style-activate "Cleveref")
      (TeX-add-symbols
       '("cref" TeX-arg-ref)
       '("Cref" TeX-arg-ref)
       '("cpageref" TeX-arg-ref)
       '("Cpageref" TeX-arg-ref)))))

;; Do not ask to save before compile
(setq compilation-ask-about-save nil)

;; Always scroll the compilation output buffer until the first error appears
(setq compilation-scroll-output 'firsterror)

;; -------------------------------------------------------------------
;; add make command to standard latex commands
;; -------------------------------------------------------------------
;; add make command to tex file
(add-hook 'LaTeX-mode-hook
   (lambda ()
     (add-to-list 'TeX-command-list
                  '("Make" "make" TeX-run-TeX nil t :help "Runs make") t)
     (add-to-list 'TeX-command-list
                  '("Scons" "scons" TeX-run-TeX nil t :help "Runs scons") t)
     (add-to-list 'TeX-command-list
                  '("latexmk" "latexmk -pdf" TeX-run-TeX nil t :help "Runs latexmk") t)))
;; -------------------------------------------------------------------
;; add new environment types to auctex
;; http://www.gnu.org/software/auctex/manual/auctex/Adding-Environments.html
;; -------------------------------------------------------------------


;; -------------------------------------------------------------------
;; Forward and inverse search with okular
;; -------------------------------------------------------------------
(use-package okular-search
  :load-path local-load-path
  :init (progn
          (setq TeX-view-program-list '(("Okular" "okular --unique %o")))
          (setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular")))

          ;; Inverse search
          ;; http://inthearmchair.wordpress.com/2010/09/02/latex-inverse-pdf-search-with-emacs/
          ;; (setq TeX-source-specials-mode 1)         ;; Inverse search

          (setq TeX-auto-global "~/.emacs.d/auctex-auto-generated-info/")
          (setq TeX-auto-local  "~/.emacs.d/auctex-auto-generated-info/")

          ;; forward search
          (add-hook 'LaTeX-mode-hook (lambda () (local-set-key "\C-c\C-a"
                                                               'okular-jump-to-line)))
          (add-hook 'tex-mode-hook (lambda () (local-set-key "\C-c\C-a"
                                                             'okular-jump-to-line)))
          )
  )

;; -------------------------------------------------------------------
;; include language tool
;; -------------------------------------------------------------------
(use-package langtool
  :defer t
  :ensure t
  :init (progn
          (setq langtool-language-tool-jar (expand-file-name "~/opt/languageTool/LanguageTool-5.0/languagetool-commandline.jar"))
          )
  :bind (
         ("C-x 4 w" . langtool-check-buffer)
         ("C-x 4 W" . langtool-check-done)
         ("C-x 4 n" . langtool-goto-next-error)
         ("C-x 4 p" . langtool-goto-previous-error)
         ("C-x 4 4" . langtool-show-message-at-point)
         )
  )

;; -------------------------------------------------------------------
;; On the fly spell checker using ispell
;; -------------------------------------------------------------------

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "de_DE") "en_US" "de_DE")))
    (ispell-change-dictionary change)
    (message "Aspell dictionary switched from %s to %s" dic change))
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "de_DE") "de-DE" "en-US")))
    (setq langtool-default-language change)
    (message "LanguageTool dictionary switched to %s" change)
    ))

(use-package ispell
  :defer t
  :ensure t
  :init (progn
          (setq ispell-dictionary "en_US")
          (setq ispell-local-dictionary "en_US")
          (setq ispell-default-dictionary "en_US")
          (setq flyspell-default-dictionary "en_US")

          (setq ispell-program-name "/usr/bin/aspell")
          (setq ispell-list-command "list")
          (setq ispell-extra-args '("--dont-tex-check-comments"))
          (setq ispell-current-dictionary "en_US")
          )
  :bind (
         ("<f4>" . fd-switch-dictionary)
         )
  )

;; alist leeren und für aspell /de_DE.UTF-8 richtig einstellen:
(setq ispell-local-dictionary-alist nil)
(add-to-list 'ispell-local-dictionary-alist
	     '("de_DE"
 	       "[[:alpha:]]" "[^[:alpha:]]"
	       "[']" t
	       ("-C" "-d" "de_DE")
 	        "~latin1" iso-8859-1)
 	     )

;; flyspell mode
(put 'LaTeX-mode 'flyspell-mode-predicate 'auctex-mode-flyspell-verify)
(defun auctex-mode-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate' in auctex mode."
  (save-excursion
    (forward-word -2)
    (not (looking-at "bibliographystyle{"))))

(add-hook 'LaTeX-mode-hook
  (lambda () (setq flyspell-generic-check-word-predicate
    'auctex-mode-flyspell-verify)))

(autoload 'flyspell-mode "flyspell"
  "On-the-fly spelling checking" t)
(autoload 'global-flyspell-mode "flyspell"
  "On-the-fly spelling" t)
(add-hook 'html-mode-hook 'flyspell-mode)
(add-hook 'htm-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; -------------------------------------------------------------------
;; Prolog
;; -------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.pl$" . prolog-mode))

;; -------------------------------------------------------------------
;; Haskell
;; -------------------------------------------------------------------
(use-package haskell-mode
  :defer t
  :ensure t
  :init (progn
          (setq haskell-program-name "ghci -XGADTs -XExistentialQuantification -XDeriveDataTypeable -XTypeFamilies")
          )
  :config (progn
             (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
             (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
            ))

;; -------------------------------------------------------------------
;; C++ integration with scons
;; -------------------------------------------------------------------

;; Interpret SConstruct file as python source file
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))

;; switch between header and source
(global-set-key [(control tab)] 'ff-find-other-file)

;; -------------------------------------------------------------------
;; Octave integration
;; -------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; -------------------------------------------------------------------
;; Python integration
;; -------------------------------------------------------------------
(defun my-elpy-mode-hook ()
   "Change elpy mode hook."
   (eldoc-mode 0)
)

(use-package elpy
  :ensure t
  :init (add-hook 'python-mode-hook #'elpy-enable)
  :config (progn
            ;; Enable Flycheck
            (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
            (add-hook 'elpy-mode-hook 'flycheck-mode)
            ;; Enable autopep
            (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
            ;; Disable eldoc due to error in melpa package
            (add-hook 'elpy-mode-hook 'my-elpy-mode-hook)
            (add-hook 'before-save-hook 'delete-trailing-whitespace)
            ;; ;; format code before save
            ;; (add-hook 'elpy-mode-hook (lambda ()
            ;;                             (add-hook 'before-save-hook
            ;;                                       'elpy-format-code nil t)))
            (setq elpy-rpc-timeout 10)
            )
  )

(add-hook 'elpy-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))

;; Enable autopep8
(use-package py-autopep8
  :ensure t
  :after elpy
  :hook ((python-mode . py-autopep8-enable-on-save))
  :config (progn
           (setq py-autopep8-options '("--select=W504 --max-line-length=120")))
  )


;; Enable python mode per default for python files
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

;; pdb debugger
(defun annotate-pdb ()
  "Colors the background if pdb is active."
  (interactive)
  (highlight-lines-matching-regexp "import ipdb")
  (highlight-lines-matching-regexp "ipdb.set_trace()"))

(add-hook 'python-mode-hook 'annotate-pdb)

;; delete output buffer on buffer execution
(setq py-shell-switch-buffers-on-execute nil)

;; install black and black-macchiato with pip3 install --user -U
;; black-macchiato black if black is not available as executable,
;; provide a dummy one that runs black in library mode
;; python3 -m black "${@}"
(use-package python-black
  :ensure t
  :after python
  :hook ((elpy-mode . python-black-on-save-mode))
)

;; -------------------------------------------------------------------
;; Sphinx documentation
;; -------------------------------------------------------------------
;; docu https://github.com/naiquevin/sphinx-doc.el
(use-package sphinx-doc
  :ensure t
  :config (progn
            (sphinx-doc-mode t)
            )
  )

;; Use C-c M-d to include doc string in python
(add-hook 'python-mode-hook (lambda ()
                              (sphinx-doc-mode t)))

;; -------------------------------------------------------------------
;; code style checker
;; -------------------------------------------------------------------
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config (progn
            (global-flycheck-mode)
            ))

(use-package blacken
  :ensure t)

(use-package clang-format
  :ensure t
  :config (progn
            (setq clang-format-executable "/usr/bin/clang-format-athena-1")
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (member major-mode '(c-mode c++-mode glsl-mode))
                          (progn
                            (when (locate-dominating-file "." ".clang-format")
                              (clang-format-buffer))
                            ;; Return nil, to continue saving.
                            nil))))
            ))

;; -------------------------------------------------------------------
;; Shell
;; -------------------------------------------------------------------
(use-package flymake-shell
  :ensure t
  :commands flymake-shell-load
  :hook ((sh-set-shell . flymake-shell-load))
  )

(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :hook ((sh-mode . flymake-shellcheck-load))
  )

;; -------------------------------------------------------------------
;; Swig-Mode
;; -------------------------------------------------------------------
(defun swig-switch-compile-command-auto ()
  (if (file-exists-p "setup.py")
      (setq compile-command "python setup.py install --install-lib=.")
    (setq compile-command "make -k"))
  (message compile-command))

;; compile command taking several compilation modes into account
(defun swig-compile()
  (interactive)
  (swig-switch-compile-command-auto)
  (compile compile-command)
)

(use-package swig-mode
  :load-path local-load-path
  :mode (("\\.i$" . swig-mode))
  :mode ()
  :init (progn
          ;; Deactivate autpair
          (add-hook 'swig-mode-hook (lambda () (setq autopair-dont-activate t)))
          )
)

;; -------------------------------------------------------------------
;; Eshell-Prompt
;; -------------------------------------------------------------------
(use-package exec-path-from-shell
  :defer t
  :ensure t
  :init (progn
          (exec-path-from-shell-initialize)
          (setq eshell-aliases-file "~/.emacs.d/local-lisp/eshell/alias")
          (setq eshell-history-size 1024)
          (setq eshell-prompt-regexp "^[^#$]*[#$] ")

          (load "em-hist")           ; So the history vars are defined
          (if (boundp 'eshell-save-history-on-exit)
             (setq eshell-save-history-on-exit t)) ; Don't ask, just save
          (if (boundp 'eshell-ask-to-save-history)
              (setq eshell-ask-to-save-history 'always)) ; For older(?) version
          )
  )


(defun eshell/ef (fname-regexp &rest dir) (ef fname-regexp default-directory))
;;; ---- path manipulation

(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
   (home-len (length home)))
    (if (and
   (>= (length pwd) home-len)
   (equal home (substring pwd 0 home-len)))
  (concat "~" (substring pwd home-len))
      pwd)))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
       PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat "["
              (if (> (length git-output) 0)
                  (substring git-output 0 -1)
                "(no branch)")
              "]") 'face `(:foreground "green"))
      )))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize ((lambda (p-lst)
            (if (> (length p-lst) 3)
                (concat
                 (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                            (substring elm 0 1)))
                            (butlast p-lst 3)
                            "/")
                 "/"
                 (mapconcat (lambda (elm) elm)
                            (last p-lst 3)
                            "/"))
              (mapconcat (lambda (elm) elm)
                         p-lst
                         "/")))
          (split-string (pwd-repl-home (eshell/pwd)) "/")) 'face `(:foreground "yellow"))
         (or (curr-dir-git-branch-string (eshell/pwd)))
         (propertize "# " 'face 'default))))

(setq eshell-highlight-prompt nil)

;; -------------------------------------------------------------------
;; Git - magit
;; -------------------------------------------------------------------
(use-package magit
  :ensure t
  :config (progn
            (setq magit-diff-refine-hunk 'all) ; Show word based diff
  ))

(use-package git-timemachine
  :ensure t)

;; -------------------------------------------------------------------
;; scala
;; -------------------------------------------------------------------
(use-package scala-mode
  :ensure t)

;; -------------------------------------------------------------------
;; protobuf mode
;; -------------------------------------------------------------------
(use-package protobuf-mode
  :ensure t)

;; -------------------------------------------------------------------
;; yaml mode
;; -------------------------------------------------------------------
(use-package yaml-mode
  :ensure t
  :config (progn
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)
            ))

;; -------------------------------------------------------------------
;; dockerfile mode
;; -------------------------------------------------------------------
(use-package dockerfile-mode
  :mode (("Dockerfile\\'" . dockerfile-mode))
  :ensure t
  :init (progn
          (put 'dockerfile-image-name 'safe-local-variable #'stringp)
          )
  )

;; ;; -------------------------------------------------------------------
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;;     (tide typescript-mode sphinx-doc markdown-mode wgrep ivy-hydra counsel smart-jump tangotango-theme pretty-lambdada magit langtool highlight-symbol helm-projectile helm-gtags helm-ag helm-R haskell-mode flycheck fill-column-indicator exec-path-from-shell cython-mode autopair auto-complete auctex anaconda-mode))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

;; -------------------------------------------------------------------
;; markdown mode
;; -------------------------------------------------------------------
(use-package markdown-mode
  :ensure t)

;; -------------------------------------------------------------------
;; RST mode
;; -------------------------------------------------------------------
(use-package poly-rst
  :ensure t
  :init (progn
          (set-default 'truncate-lines t)
          )
  :mode (
         ("\\.rst$" . poly-rst-mode)
         ("\\.rest$" . poly-rst-mode)
         )
  )

(use-package rst
  :ensure t
  :mode (
         ("\\.txtt$" . rst-mode)
         ("\\.rst$" . rst-mode)
         ("\\.rest$" . rst-mode)
         )
  )

;; -------------------------------------------------------------------
;; Typescript
;; -------------------------------------------------------------------
(use-package typescript-mode
  :mode (
         ("\\.ts$" . typescript-mode)
         ("\\.tsx$" . typescript-mode)
         )
  :ensure t
  :config (progn
            (flycheck-mode +1)
            (company-mode +1)
            )
  )

(use-package tide
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

(use-package json-snatcher
  :ensure t
  :hook ((js-mode-hook . js-mode-bindings)
         (js2-mode-hook . js-mode-bindings))
  :bind (
         ("C-C C-g" . jsons-print-path)
        )
  )

;; -------------------------------------------------------------------
;; Org + Plantuml mode
;; -------------------------------------------------------------------
(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure t
  :config
  (progn
    ;; config stuff
    (setq org-plantuml-jar-path (expand-file-name "/lhome/franzef/opt/plantuml/plantuml.jar"))
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
    (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
    ))

(use-package flycheck-plantuml
  :ensure t
  )

(use-package plantuml-mode
  :mode (("\\.puml" . plantuml-mode)
         ("\\.iuml" . plantuml-mode)
         ("\\.uml" . plantuml-mode))
  :ensure t
  :config (progn
            ;; Sample jar configuration
            (setq plantuml-jar-path (expand-file-name "~/opt/plantuml/plantuml.jar"))
            (setq plantuml-default-exec-mode 'jar)
            ;; Open in same window
            (add-to-list 'display-buffer-alist
                         '(progn
                            (get-buffer-create "*PLANTUML Preview*")
                            '((display-buffer-below-selected display-buffer-at-bottom)
                              (inhibit-same-window . t)
                              (window-height . fit-window-to-buffer))))
            )
  )

;; -------------------------------------------------------------------
;; Groovy mode for Jenkins
;; -------------------------------------------------------------------
(use-package groovy-mode
  :mode (("\\.groovy$" . groovy-mode))
  :ensure t)

(use-package groovy-imports
  :ensure t)

;; -------------------------------------------------------------------
;; Show number of lines in the left side of the buffer
;; -------------------------------------------------------------------
(use-package linum+
  :load-path local-load-path
  :hook ((python-mode . linum-mode)
         (ess-mode . linum-mode)
         (c-mode . linum-mode)
         (c++-mode . linum-mode)
         (octave-mode . linum-mode)
         (sphinx-doc-mode . linum-mode)
         (markdown-mode . linum-mode)
         (poly-rst-mode . linum-mode)
         (cmake-mode . linum-mode)
         (elpy-mode . linum-mode)
         (typescript-mode . linum-mode)
         (plantuml-mode . linum-mode)
         (java-mode . linum-mode)
         (sh-mode . linum-mode)
         (js-mode . linum-mode)
         (json-mode . linum-mode)
         )
  )

;; -------------------------------------------------------------------
;; Java mode
;; -------------------------------------------------------------------
(use-package meghanada
  :ensure t
  :hook ((java-mode .
                    (lambda ()
                      ;; meghanada-mode on
                      (meghanada-mode t)
                      (flycheck-mode +1)
                      (setq c-basic-offset 2)
                      ;; use code format
                      (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
         )
  :config (cond
           ((eq system-type 'windows-nt)
            (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
            (setq meghanada-maven-path "mvn.cmd"))
           (t
            (setq meghanada-java-path "java")
            (setq meghanada-maven-path "mvn")))
  )

;; -------------------------------------------------------------------
;; Json
;; -------------------------------------------------------------------
(use-package json-mode
  :ensure t
  :mode (("\\.json$" . json-mode))
  )

;; make sure that you have jsonlint installed: sudo env "PATH=$PATH"
;; npm install jsonlint -g
(use-package flymake-json
  :ensure t
  :requires json-mode
  :requires flymake-easy flymake-haml
  :hook ((json-mode . flymake-json-load)
         (js-mode . flymake-json-maybe-load))
  )

;; -------------------------------------------------------------------
;; Other stuff
;; -------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (python-black scala-mode ess flycheck-clang-tidy helm-mt multi-term winner-mode dockerfile-mode groovy-imports groovy-mode flycheck-plantuml plantuml-mode org-mode poly-rst rst-mode yaml-mode whole-line-or-region wgrep volatile-highlights use-package tide tangotango-theme sphinx-doc smart-jump python-mode py-autopep8 protobuf-mode neotree markdown-mode magit langtool ivy-rtags ivy-hydra highlight-symbol helm-projectile helm-gtags helm-ag helm-R haskell-mode git-timemachine flycheck-rtags fill-column-indicator exec-path-from-shell ensime elpy dired-narrow diminish cython-mode crux counsel cmake-mode clang-format blacken beacon autopair auto-complete auctex anaconda-mode ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; .emacs ends here
