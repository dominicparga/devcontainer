;;; .emacs --- this file starts here

;;; Commentary:
;; inspired by https://github.com/daviwil/dotfiles/blob/master/Emacs.org

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

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(defvar local-load-path (expand-file-name "~/.emacs.d/local-lisp"))
(add-to-list 'load-path local-load-path)

;; Initialise packages
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; ensure all packages to be installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; auto update packages
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; add the possibility to define system dependencies in use-package
;; declaration
(use-package use-package-ensure-system-package)

;; make sure that the path environment from shell is available in
;; emacs
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize))
  )

;; ===================================================================
;; Basic Settings
;; ===================================================================

(server-start)

;; library for defining prefixed keybindings
;; https://github.com/noctuid/general.el
(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer dw/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer dw/ctrl-c-keys
    :prefix "C-c"))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;disable backup
(setq backup-inhibited t)
(setq make-backup-files nil)

;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;disable auto save
(setq auto-save-default nil)

;; dont warn for following symlinked files
(setq vc-follow-symlinks t)

;; disable scrollbar
(scroll-bar-mode -1)

;; disable menu
(menu-bar-mode nil)

;; enable revert from disk
(global-auto-revert-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable ahead-of-time compliation when installing packages
;; https://www.emacswiki.org/emacs/GccEmacs
(setq package-native-compile t)

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

;; set unicode encoding
(defvar prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'latin-1)
(setq buffer-file-coding-system 'utf-8)

;; no splash screen
(setq inhibit-splash-screen t)

;; no backup files
(setq make-backup-files nil)

;; Color theme
(use-package material-theme)

(load-theme 'material t)

;; show logs of executed commands
(use-package command-log-mode)

;; remove toolbar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Key bindings
(global-set-key "\C-n" 'make-frame)

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
(setq-default tab-width 4)

;; Delete trailing white spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; It reverts when a dired buffer is reselected dired mode
(use-package dired
  :ensure nil
  :hook (dired-mode . auto-revert-mode)
  :config
  (setq dired-auto-revert-buffer t ; Auto update when buffer is revisited
        dired-dwim-target t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        delete-by-moving-to-trash t
        dired-listing-switches "-alh") ; human readable file sizes
  )

;; Quick filter dired view
(use-package dired-narrow
  :bind (:map dired-mode-map ("/" . dired-narrow)))

;; Let kill operate on the whole line when no region is selected
(use-package whole-line-or-region
  :config (whole-line-or-region-global-mode))

;; Remove indicators from the mode line
(use-package diminish)

;; Helps to keep track of your cursor
(use-package beacon
  :config
  (beacon-mode t)
  (setq beacon-color "#ff0000"))

;; volatile highlights - temporarily highlight changes from pasting
;; etc
(use-package volatile-highlights
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

;; set zsh as default shell name
(setq shell-file-name "/bin/zsh")

;; enable emojis in buffers
(use-package emojify
  :hook (after-init . global-emojify-mode))

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; -------------------------------------------------------------------
;; Credential management
;; -------------------------------------------------------------------

(use-package ivy-pass
  :commands ivy-pass
  :config
  (setq password-store-password-length 12))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

;; -------------------------------------------------------------------
;; Open files externally
;; -------------------------------------------------------------------
(use-package openwith
  :config
  (setq openwith-associations
    (list
      (list (openwith-make-extension-regexp
             '("mpg" "mpeg" "mp3" "mp4"
               "avi" "wmv" "wav" "mov" "flv"
               "ogm" "ogg" "mkv"))
             "mpv"
             '(file))
      (list (openwith-make-extension-regexp
             '("xbm" "pbm" "pgm" "ppm" "pnm"
               "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
                                                ;; causing feh to be opened...
             "feh"
             '(file))
      (list (openwith-make-extension-regexp
             '("pdf"))
             "zathura"
             '(file))))
  (openwith-mode 1))

;; -------------------------------------------------------------------
;; Copy & Paste
;; -------------------------------------------------------------------

;; ;; (transient-mark-mode 1) ; Now on by default: makes the region act
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
;; Snippets
;; -------------------------------------------------------------------

(use-package yasnippet
  :config (yas-global-mode 1)
  )

;; -------------------------------------------------------------------
;; Auto Completion
;; -------------------------------------------------------------------
(use-package auto-complete)

;; -------------------------------------------------------------------
;; Fill Column Indicator
;; -------------------------------------------------------------------
(use-package fill-column-indicator
  :init
  (setq fci-rule-color "#f8f8f8") ;;#cccccc
  (define-globalized-minor-mode
    global-fci-mode fci-mode (lambda () (fci-mode 1)))
  )

;; -------------------------------------------------------------------
;; Beautify modline
;; -------------------------------------------------------------------
(use-package diminish)

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'respectful)  ; Respect the theme colors
  (setq sml/mode-width 'right
      sml/name-width 60)

  (setq-default mode-line-format
  `("%e"
      mode-line-front-space
      evil-mode-line-tag
      mode-line-mule-info
      mode-line-client
      mode-line-modified
      mode-line-remote
      mode-line-frame-identification
      mode-line-buffer-identification
      sml/pos-id-separator
      (vc-mode vc-mode)
      " "
      ;mode-line-position
      sml/pre-modes-separator
      mode-line-modes
      " "
      mode-line-misc-info))

  (setq rm-excluded-modes
    (mapconcat
      'identity
      ; These names must start with a space!
      '(" GitGutter" " MRev" " company"
      " Helm" " Undo-Tree" " Projectile.*" " Z" " Ind"
      " Org-Agenda.*" " ElDoc" " SP/s" " cider.*")
      "\\|")))


;; -------------------------------------------------------------------
;; Insert Pairs of Matching Elements
;; -------------------------------------------------------------------
(use-package autopair
  :init
  (autopair-global-mode t)
  :config
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

;; Highlight parens
(use-package paren
  :config
  (setq show-paren-style 'mixed)	;; The entire expression
  (setq blink-matching-paren t)
  :init
  (show-paren-mode 1)
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "#def")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold))

;; -------------------------------------------------------------------
;; Save history during sessions
;; -------------------------------------------------------------------
(use-package savehist
  :config
  (savehist-mode t)
  (setq savehist-additional-variables '(extended-command-history kill-ring)))

;; -------------------------------------------------------------------
;; Helm/Ivy project
;; -------------------------------------------------------------------
;; (use-package setup-helm
;;   :load-path local-load-path
;;   )

(use-package setup-ivy
 :load-path local-load-path
 )

;; -------------------------------------------------------------------
;; Projectile mode
;; -------------------------------------------------------------------
(use-package projectile
  :after counsel
  :init
  (setq projectile-file-exists-remote-cache-expire nil)
  (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
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
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :bind* (
          ("C-c p s a" . counsel-ack)
          ("C-c p s g" . counsel-git-grep)
          ("C-c p s r" . rgrep)
          )
  )

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))


;; -------------------------------------------------------------------
;; highlight symbol and replace
;; -------------------------------------------------------------------
(use-package highlight-symbol
  :bind* (
         ("C-<f3>" . highlight-symbol)
         ("<f3>" . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-query-replace)
         )
  )

;; -------------------------------------------------------------------
;; Enable lsp and treemacs
;; -------------------------------------------------------------------

(use-package lsp-mode
  :commands lsp
  :ensure-system-package
  ((clangd . "apt install clangd"))
  :hook ((lsp-mode . (lambda ()
                       (let ((lsp-keymap-prefix "C-c l"))
                         (lsp-enable-which-key-integration))))
         (c++-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         )
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-ui-doc-position 'top
        lsp-ui-doc-alignment 'window
        ;; if set to true can cause a performance hit
        lsp-log-io nil
        lsp-pyls-plugins-flake8-config (expand-file-name "~/.flake8")
        lsp-pyls-plugins-flake8-enabled t
        lsp-pyls-plugins-pycodestyle-enabled nil
        ;; lsp-enable-snippet nil
        ;; lsp-prefer-flymake :none))
        ;; lsp-enable-snippet nil
        ;; Ignore files and folders when watchin
        ;; lsp-file-watch-ignored ("[/\\\\]\\.pyc$" "[/\\\\]_build")
        ))

;; increase threshold for lsp to run smoothly
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-completion-provider :capf)

(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (setq imenu-auto-rescan t
        imenu-auto-rescan-maxout (* 1024 1024)
        imenu--rescan-item '("" . -99))
  )

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode 1)
  :config
  (setq which-key-idle-delay 0.5))

(use-package treemacs
  :commands (treemacs
             treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode)
  :bind (("<f8>" . treemacs)
         ("<f9>" . treemacs-select-window))
  :init
  (when window-system
    (setq treemacs-width 27
          treemacs-is-never-other-window t
          treemacs-indentation 0
          treemacs-space-between-root-nodes nil)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode nil)))

(use-package lsp-treemacs
  :after lsp
  :after treemacs
  :after company
  :commands lsp-treemacs-errors-list
  :config (setq gc-cons-threshold (* 100 1024 1024)
                read-process-output-max (* 1024 1024)
                treemacs-space-between-root-nodes nil
                company-idle-delay 0.0
                company-minimum-prefix-length 1
                lsp-idle-delay 0.1 ;; clangd is fast
                ;; be more ide-ish
                lsp-headerline-breadcrumb-enable))

;; -------------------------------------------------------------------
;; Enable dap
;; -------------------------------------------------------------------
(use-package dap-mode
  :after lsp-mode
  :ensure-system-package ("~/.local/lib/python3.8/site-packages/ptvsd" . "pip3 install 'ptvsd>=4.2'") ; for dap-python
  :ensure-system-package ("~/.local/lib/python3.8/site-packages/pyls" . "pip3 install pyls") ; for dap-python
  :config (dap-auto-configure-mode)
  :init
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1)
  :hook ((dap-stopped . (lambda (arg) (call-interactively #'dap-hydra))))
  )

;; ===================================================================
;; Adjusting modes for programming
;; ===================================================================
;; -------------------------------------------------------------------
;; Eshell
;; -------------------------------------------------------------------
(use-package setup-eshell
  :load-path local-load-path
  )


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
  :ensure-system-package zsh
  :config
  (setq multi-term-program "/bin/zsh")
  (setq explicit-shell-file-name "/bin/zsh")
  )

(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-y") 'term-paste))

;; -------------------------------------------------------------------
;; C++
;; -------------------------------------------------------------------
(use-package cmake-mode
  :mode (("\\.cmake$" . cmake-mode)
         ("CMakeLists.txt" . cmake-mode)))

;; switch between header and source
(global-set-key [(control tab)] 'ff-find-other-file)

(use-package company
  :config
  (setq company-backends
        (delete 'company-semantic company-backends))
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0) ;; default is 0.2
  (global-company-mode 1)
  )

;; disable company mode for terminals
(dolist (mode '(term-mode-hook
                multi-term-mode-hook
                ansi-term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (company-mode 0))))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

(with-eval-after-load 'lsp-mode
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace))

(use-package company-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/9/")
  )

(use-package clang-format+
  :hook (c++-mode . clang-format+-mode)
  :config
  (let ((athena_clang "/usr/bin/clang-format-athena-1"))
    (when (file-exists-p athena_clang)
      (setq clang-format-executable athena_clang)
      )
    )
  )

;; Interpret SConstruct file as python source file
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))

;; -------------------------------------------------------------------
;; CRAN R
;; -------------------------------------------------------------------
(use-package ess
  :init
  (setq ess-use-eldoc nil)
  ;; ESS will not print the evaluated commands, also speeds up the evaluation
  (setq ess-eval-visibly nil)
  ;; if you don't want to be prompted each time you start an interactive R session
  (setq ess-ask-for-ess-directory nil)
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
  :hook (
         (LaTeX-mode . TeX-fold-mode)
         (LaTeX-mode . outline-minor-mode)
         )
  )

(use-package company-auctex
  :after company)

(use-package reftex
  :init
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

  (setq LaTeX-command "latex -synctex=1") ;; enable synctex
  :config
  ;; Set index on document
  (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
  (add-hook 'reftex-mode-hook 'imenu-add-menubar-index)
  :hook (
         (reftex-mode . imenu-add-menubar-index)
         (LaTeX-mode . turn-on-reftex)
         (latex-mode . turn-on-reftex)
         (LaTeX-mode . reftex-mode)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . TeX-PDF-mode)
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

;; Always scroll the compilation output buffer until the first error
;; appears
(setq compilation-scroll-output 'firsterror)

;; add make, scons and latexmk commands as tex build commands
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
  :ensure-system-package okular
  :bind (:map LaTeX-mode-map
              ("C-c C-a" . okular-jump-to-line)
              :map tex-mode-map
              ("C-c C-a" . okular-jump-to-line)
              )
  :init
  (setq TeX-view-program-list '(("Okular" "okular --unique %o")))
  (setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular")))

  ;; Inverse search
  ;; http://inthearmchair.wordpress.com/2010/09/02/latex-inverse-pdf-search-with-emacs/
  ;; (setq TeX-source-specials-mode 1)         ;; Inverse search

  (setq TeX-auto-global "~/.emacs.d/auctex-auto-generated-info/")
  (setq TeX-auto-local  "~/.emacs.d/auctex-auto-generated-info/")
  )

;; -------------------------------------------------------------------
;; include language tool
;; -------------------------------------------------------------------
(defun install-language-tool (version)
  "Downloads and installs the language tool. This is an external
dependency of the langtool package."
  (let* ((name (concat "LanguageTool-" version))
         (url (concat "https://languagetool.org/download/" name ".zip"))
         (path (expand-file-name "~/opt/languageTool"))
         (target (concat "/tmp/" name ".zip"))
         (jar (concat path "/" name "/languagetool-commandline.jar")))
    (unless (file-directory-p path) (make-directory path))
    (unless  (file-exists-p jar)
      (unless (file-exists-p target)
        (message (concat "[langtool] Downloading " name))
        (url-copy-file url target))
      (message (concat "[langtool] Decompress " name))
      (call-process-shell-command (concat "unzip " target " -d " path) nil 0)
      )
    (if (file-exists-p jar) jar nil)
    ))

(defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g' .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))

(use-package langtool
  :init
  (setq langtool-version "5.2")
  (setq langtool-language-tool-jar (install-language-tool langtool-version))
  :config
  (setq langtool-autoshow-message-function 'langtool-autoshow-detail-popup)
  (setq langtool-language-tool-jar (install-language-tool langtool-version))
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
  "Switch dictionary from American English to German an vice versa."
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "de_DE") "en_US" "de_DE")))
    (ispell-change-dictionary change)
    (message "[ispell] Dictionary switched from %s to %s" dic change))
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "de_DE") "de-DE" "en-US")))
    (setq langtool-default-language change)
    (message "[langtool] Dictionary switched to %s" change)
    ))

(use-package ispell
  :ensure-system-package
  ((ispell . "apt install ispell iogerman ingerman ienglish-common iamerican-insane"))
  :init
  (setq ispell-dictionary "en_US")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-default-dictionary "en_US")
  (setq flyspell-default-dictionary "en_US")

  (setq ispell-program-name "/usr/bin/aspell")
  (setq ispell-list-command "list")
  (setq ispell-extra-args '("--dont-tex-check-comments"))
  (setq ispell-current-dictionary "en_US")
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
;; Python
;; -------------------------------------------------------------------

(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

;; Enable autopep8
(use-package py-autopep8
  :hook ((python-mode . py-autopep8-enable-on-save))
  :config
  (setq py-autopep8-options '("--select=W504 --max-line-length=120")))

;; Enable python mode per default for python files
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

;; pdb debugger
(defun annotate-pdb ()
  "Colors the background if pdb is active."
  (interactive)
  (highlight-lines-matching-regexp "import ipdb")
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "ipdb.set_trace()"))

(add-hook 'python-mode-hook 'annotate-pdb)

;; delete output buffer on buffer execution
(setq py-shell-switch-buffers-on-execute nil)

;; install black and black-macchiato with pip3 install --user -U
;; black-macchiato black if black is not available as executable in
;; ~/.local/bin, provide a dummy one that runs black in library mode
;; python3 -m black "${@}"
(use-package python-black
  :after python
  :ensure-system-package (black . "pip3 install --user -U black")
  :hook ((python-mode . python-black-on-save-mode))
  )

(use-package conda
  :config
  (setq conda-anaconda-home (expand-file-name "~/anaconda3/"))
  (setq conda-env-home-directory (expand-file-name "~/anaconda3"))
)

;; install via pip install "ptvsd>=4.2"
(require 'dap-python)

;; supports virtual environments. To be set with pyvenv-workon
(use-package pyvenv)

;; -------------------------------------------------------------------
;; Sphinx documentation
;; -------------------------------------------------------------------
;; docu https://github.com/naiquevin/sphinx-doc.el
;; Use C-c M-d to include doc string in python
(use-package sphinx-doc
  :hook ((python-mode . sphinx-doc-mode))
  :config
  (sphinx-doc-mode t)
  )

;; -------------------------------------------------------------------
;; code style checker
;; -------------------------------------------------------------------
(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode)
)

(use-package blacken)

;; -------------------------------------------------------------------
;; Shell
;; -------------------------------------------------------------------
(use-package flymake-shell
  :commands flymake-shell-load
  :hook ((sh-set-shell . flymake-shell-load))
  )

(use-package flymake-shellcheck
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
  :init
  ;; Deactivate autpair
  (add-hook 'swig-mode-hook (lambda () (setq autopair-dont-activate t)))
)

;; -------------------------------------------------------------------
;; Git - magit
;; -------------------------------------------------------------------
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (setq magit-diff-refine-hunk 'all) ; Show word based diff
)

;; (dw/leader-key-def
;;   "g"   '(:ignore t :which-key "git")
;;   "gs"  'magit-status
;;   "gd"  'magit-diff-unstaged
;;   "gc"  'magit-branch-or-checkout
;;   "gl"   '(:ignore t :which-key "log")
;;   "glc" 'magit-log-current
;;   "glf" 'magit-log-buffer-file
;;   "gb"  'magit-branch
;;   "gP"  'magit-push-current
;;   "gp"  'magit-pull-branch
;;   "gf"  'magit-fetch
;;   "gF"  'magit-fetch-all
;;   "gr"  'magit-rebase)

(use-package git-timemachine)

(use-package magit-todos)

;; -------------------------------------------------------------------
;; scala
;; -------------------------------------------------------------------
(use-package scala-mode)

;; -------------------------------------------------------------------
;; protobuf mode
;; -------------------------------------------------------------------
(use-package protobuf-mode)

;; -------------------------------------------------------------------
;; yaml mode
;; -------------------------------------------------------------------
(use-package yaml-mode
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode))
  :bind (:map yaml-mode-map ("C-m/" . newline-and-indent)))

;; -------------------------------------------------------------------
;; dockerfile mode
;; -------------------------------------------------------------------
(use-package dockerfile-mode
  :mode (("Dockerfile\\'" . dockerfile-mode))
  :init
  (put 'dockerfile-image-name 'safe-local-variable #'stringp)
  )

(use-package lsp-docker)

;; -------------------------------------------------------------------
;; markdown mode
;; -------------------------------------------------------------------
(use-package markdown-mode)

;; -------------------------------------------------------------------
;; RST mode
;; -------------------------------------------------------------------
(use-package poly-rst
  :mode (
         ("\\.rst$" . poly-rst-mode)
         ("\\.rest$" . poly-rst-mode)
         )
  :init
  (set-default 'truncate-lines t)
  )


;; C-c C-e r r (org-rst-export-to-rst)
;;    Export as a text file written in reStructured syntax.
;; C-c C-e r R (org-rst-export-as-rst)
;;    Export as a temporary buffer. Do not create a file.
(use-package ox-rst)

(use-package rst
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
  :after dap-node
  :mode (
         ("\\.ts$" . typescript-mode)
         ("\\.tsx$" . typescript-mode)
         )
  :config
  (flycheck-mode 1)
  (company-mode 1)
  (dap-node-setup) ;; automatically installs Node debug adapter if needed
  )

(use-package tide)

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
  :config
    ;; config stuff
    (setq org-plantuml-jar-path (expand-file-name "~/opt/plantuml/plantuml.jar"))
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
    (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
    )

(use-package flycheck-plantuml
  :after flycheck
  )

(use-package plantuml-mode
  :mode (("\\.puml" . plantuml-mode)
         ("\\.iuml" . plantuml-mode)
         ("\\.uml" . plantuml-mode))
  :config
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

;; -------------------------------------------------------------------
;; Groovy mode for Jenkins
;; -------------------------------------------------------------------
(use-package groovy-mode
  :mode (("\\.groovy$" . groovy-mode)))

(use-package groovy-imports)

;; -------------------------------------------------------------------
;; Show number of lines in the left side of the buffer
;; -------------------------------------------------------------------
(column-number-mode 1)
(global-display-line-numbers-mode 1)

(dolist (mode '(org-mode-hook
                term-mode-hook
                multi-term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; enable rainbow delimiters for all programming-modes (prog-mode)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; -------------------------------------------------------------------
;; Java mode
;; -------------------------------------------------------------------
(use-package lsp-java
  :after lsp-mode
  :hook ((java-mode . lsp)))

;; -------------------------------------------------------------------
;; Json
;; -------------------------------------------------------------------
(use-package json-mode
  :mode (("\\.json$" . json-mode))
  )

;; make sure that you have jsonlint installed: sudo env "PATH=$PATH"
;; npm install jsonlint -g
(use-package flymake-json
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
 '(custom-safe-themes
   '("d4f8fcc20d4b44bf5796196dbeabec42078c2ddb16dcb6ec145a1c610e0842f3" default))
 '(package-selected-packages
   '(multi-eshell lsp-ivy which-key yasnippet use-package-ensure-system-package rainbow-delimiters command-log-mode company-prescient ivy-prescient emojify xterm-color evil-collection ivy-posframe smex ivy-rich eshell-z general openwith ivy-pass evil-nerd-commenter smart-mode-line dap-node pyvenv jedi dap-mode lsp-docker lsp-java lsp-mode lsp-ui helm-swoop quelpa quelpa-use-package python-black meghanada scala-mode ess flycheck-clang-tidy helm-mt multi-term winner-mode dockerfile-mode groovy-imports groovy-mode flycheck-plantuml plantuml-mode org-mode poly-rst rst-mode yaml-mode whole-line-or-region wgrep volatile-highlights use-package tide tangotango-theme sphinx-doc smart-jump python-mode py-autopep8 protobuf-mode neotree markdown-mode magit langtool ivy-rtags ivy-hydra highlight-symbol helm-projectile helm-ag helm-R haskell-mode git-timemachine flycheck-rtags fill-column-indicator exec-path-from-shell ensime elpy dired-narrow diminish cython-mode crux counsel cmake-mode clang-format blacken beacon autopair auto-complete auctex anaconda-mode ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; .emacs ends here
