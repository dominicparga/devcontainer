;;; init.el --- this file starts here

;;; Commentary:
;; inspired by https://github.com/daviwil/dotfiles/blob/master/Emacs.org
;; and https://git.sr.ht/~sirn/dotfiles/tree/217c239cf19b668deaccc40ad76c60ffbcfdad20/item/etc/emacs/packages

;;; Code:

;; ==================================================================
;; Load Packages for different modes
;; ===================================================================
(setq user-full-name "Fabian Franzelin"
      user-mail-address "fabian.franzelin@de.bosch.com"
      inhibit-startup-echo-area-message (getenv "USER"))

;; Package Management
(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives '(("org" . "https://orgmode.org/elpa/") ;; will be deprecated soon
                         ("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialise packages
(when (< emacs-major-version 27)
  (package-initialize))

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
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t
        ;; update the packages every week
        auto-package-update-interval 7)
  (auto-package-update-maybe))

;; add the possibility to define system dependencies in use-package
;; declaration
(use-package use-package-ensure-system-package)

;; get latest signatures for elpa
(use-package gnu-elpa-keyring-update)

;; make sure that the path environment from shell is available in
;; emacs
(use-package exec-path-from-shell
  :unless (string-equal system-type "windows-nt")
  :demand t
  :init
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(defun ff/emacs-config-home ()
  "Provide the home of the Emacs configuration folder."
  (interactive)
  (let* ((xdg-config-home (getenv "XDG_CONFIG_HOME"))
         (emacs-d-folder (expand-file-name "~/.emacs.d")))
    (if xdg-config-home
	    (let ((xdg-emacs-config-home (concat xdg-config-home "/emacs")))
          (if (file-directory-p xdg-emacs-config-home) xdg-emacs-config-home emacs-d-folder))
      emacs-d-folder)))

(defvar emacs-config-home (ff/emacs-config-home)
  "Location of the Emacs configuration.")
(defvar local-load-path (concat emacs-config-home "/elisp-local")
  "Load path for local Emacs configurations.")
(add-to-list 'load-path local-load-path)

(use-package helpers
  :load-path local-load-path)

(use-package shell-loader
  :load-path local-load-path)

;; ===================================================================
;; Basic Settings
;; ===================================================================

(server-start)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; enables local variables per default
(setq enable-local-variables t)
;; dir-local variables will be applied to remote files.
(setq enable-remote-dir-locals t)

;; disable backup
(setq backup-inhibited t)
(setq make-backup-files nil)

;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; enable auto save
(setq auto-save-default t)

;; dont warn for following symlinked files
(setq vc-follow-symlinks t)

;; disable lockfiles
(setq create-lockfiles nil)

;; setup a new custom file
(setq emacs-d-custom (concat (ff/emacs-config-home) "/custom.el"))
(unless (file-exists-p emacs-d-custom)
  (with-temp-buffer (write-file emacs-d-custom)))

(setq custom-file emacs-d-custom)
(load custom-file)

;; disable scrollbar
(scroll-bar-mode -1) ; disbale scrollbar
(menu-bar-mode -1) ; disable menu bar
(tool-bar-mode -1) ; disbale tool bar
(set-fringe-mode 10)

;; disable scroll lock mode permanently
(defun do-nothing () (interactive))
(global-set-key (kbd "<Scroll_Lock>") 'do-nothing)
(setq scroll-lock-mode nil)

;; enable revert from disk
(global-auto-revert-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; let me confirm emacs before killing it
(setq confirm-kill-emacs 'yes-or-no-p)

;; Enable ahead-of-time compliation when installing packages
;; https://www.emacswiki.org/emacs/GccEmacs
(setq package-native-compile t)

;; Support wheel mouse scrolling
(mouse-wheel-mode t)

;; Kill this buffer, instead of prompting for which one to kill
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Turn on syntax colouring in all modes supporting it
(global-font-lock-mode t)

;; I want the current user name, the emacs version and the name of the
;; file I'm editing to be displayed in the title-bar.
(setq frame-title-format
      (list (getenv "USER") "@Emacs " emacs-version ": "
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; set unicode encoding
(defvar prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'latin-1)
(setq buffer-file-coding-system 'utf-8)

;; no splash screen
(setq inhibit-splash-screen t)

;; increase undo limit
(setq undo-limit 8000000)

;; Iterate through CamelCase
(global-subword-mode t)

;; Color theme
(use-package material-theme
  :demand t
  :config
  (load-theme 'material t))

;; show logs of executed commands
(use-package command-log-mode)

;; Key bindings
(global-set-key "\C-n" 'make-frame)

;; Ctrl-1 stores a position in a file, Alt-1 brings you back to this position
(global-set-key (kbd "C-1")  '(lambda () (interactive) (point-to-register ?1)))
(global-set-key (kbd "M-1")  '(lambda () (interactive) (register-to-point ?1)))

;; higlight the marked region (C-SPC) and use commands (like
;; latex-environment) on current region.
(transient-mark-mode t)

;; enable cua mode to mark text vertically
(cua-mode)

;; Indentation
(setq-default indent-tabs-mode nil)    ; use only spaces and no tabs
(setq-default tab-width 4)

;; Always split the frame vertically and never horizontally
;; (setq split-width-threshold 0)
;; (setq split-height-threshold nil)

;; Delete trailing white spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; enable auto pair mode globally
(electric-pair-mode t)

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

;; setup all the icons with fix for fonts from
;; https://github.com/domtronn/all-the-icons.el/issues/107
(require 'font-lock)
(use-package font-lock+
  :load-path local-load-path
  )

;; NOTE: The first time you load your configuration on a new machine,
;; you’ll need to run `M-x all-the-icons-install-fonts` so that mode
;; line icons display correctly.
(use-package all-the-icons
  :after font-lock+)

;; Google search integration
(use-package google-this
  :init
  (google-this-mode t)
  :config
  (global-set-key (kbd "C-c g") 'google-this-mode-submap)
  )

;; -------------------------------------------------------------------
;; Tramp
;; -------------------------------------------------------------------
(use-package tramp
  :ensure nil
  :config
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-use-ssh-controlmaster-options nil)
  (setq tramp-auto-save-directory "~/.cache/emacs/backups")
  (setq tramp-persistency-file-name "~/.emacs.d/data/tramp")
  (setq tramp-terminal-type "dumb")
  (setq tramp-default-method "ssh"))

(use-package docker-tramp)

(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode 1)
  :config
  (setq which-key-idle-delay 0.5))

;; -------------------------------------------------------------------
;; Tab bar
;; -------------------------------------------------------------------
(use-package tab-bar
  :config
  ;; Don't turn on tab-bar-mode when tabs are created
  (setq tab-bar-show nil)
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil))

;; -------------------------------------------------------------------
;; Ivy project
;; ------------------------------------------------------------------
(use-package setup-ivy
  :load-path local-load-path)

;; -------------------------------------------------------------------
;; Company
;; -------------------------------------------------------------------
(use-package company
  :after (lsp-mode yasnippet)
  :hook (lsp-mode . company-mode)
  :config
  (setq company-backends (delete 'company-semantic company-backends)
        company-minimum-prefix-length 1
        company-idle-delay 0.2 ;; default is 0.2
        company-echo-delay 0.2
        ;; aligns annotation to the right hand side
        company-tooltip-align-annotations t)

  ;; The company-backends support list of lists. Lists are evaluated
  ;; at once, which
  (defvar +ff/company-default-backends '((company-capf
                                          company-files
                                          :with company-yasnippet)
                                         (company-abbrev
                                          company-ispell)))
  (setq company-backends +ff/company-default-backends)

  ;; enable company globally
  (global-company-mode 1)
  :bind (("C-c C-y" . company-yasnippet)
         :map company-active-map
         ("TAB" . company-complete-selection)))


;; disable company mode for terminals
(dolist (mode '(term-mode-hook
                vterm-mode-hook
                eshell-mode-hook
                dap-ui-repl-mode-hook))
  (add-hook mode (lambda () (company-mode 0))))

(use-package company-prescient
  :config
  (company-prescient-mode 1))

(use-package company-quickhelp
  :after company)

;; -------------------------------------------------------------------
;; Simple text
;; -------------------------------------------------------------------
(defun ff/configure-text-mode ()
  "Configure text mode."
  (interactive)
  (define-key text-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
  (set (make-local-variable 'company-backends)
       '((company-abbrev
          company-dabbrev
          company-ispell
          company-files
          :with company-yasnippet)
         )))

(use-package text-mode
  :ensure nil
  :after company
  :hook (text-mode . ff/configure-text-mode))

;; -------------------------------------------------------------------
;; Buffer move & transpose frame
;; -------------------------------------------------------------------
(use-package buffer-move
  :bind (("C-c b <down>" . buf-move-down)
         ("C-c b <up>" . buf-move-up)
         ("C-c b <left>" . buf-move-left)
         ("C-c b <right>" . buf-move-right)))

(use-package transpose-frame
  :bind (("C-c b t" . transpose-frame)))

;; -------------------------------------------------------------------
;; Set up dired
;; -------------------------------------------------------------------
(use-package dired
  :ensure nil
  :defer 1
  :commands (dired dired-jump)
  :hook ((dired-mode . auto-revert-mode)
         (dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("<backspace>" . dired-up-directory)
         ("TAB" . dired-find-file))
  :custom
  ((dired-auto-revert-buffer t) ; Auto update when buffer is revisited
   (dired-dwim-target t)
   (dired-recursive-deletes 'always)
   (dired-recursive-copies 'always)
   (delete-by-moving-to-trash t))
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-hide-details-hide-symlink-targets nil)

  (autoload 'dired-omit-mode "dired-x"))

(use-package dired-rainbow
  :defer 2
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :bind (:map dired-mode-map
              ("H" . dired-hide-dotfiles-mode)))

;; -------------------------------------------------------------------
;; Undo tree - make undos more powerful
;; -------------------------------------------------------------------
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; -------------------------------------------------------------------
;; Credential management
;; -------------------------------------------------------------------
(use-package ivy-pass
  :commands ivy-pass
  :config
  (setq password-store-password-length 20))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

;; -------------------------------------------------------------------
;; Open files externally
;; -------------------------------------------------------------------
(use-package openwith
  :ensure-system-package ((vlc . vlc)
                          (okular . okular)
                          (eog . eog)
                          (firefox . firefox))
  :init
  (openwith-mode t)
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "vlc"
               '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg"
                  "jpg")) ;; Removed jpg because Telega was
               ;; causing feh to be opened...
               "eog"
               '(file))
         (list (openwith-make-extension-regexp
                '("pdf"))
               "okular"
               '(file))
         (list (openwith-make-extension-regexp
                '("html"))
               "firefox"
               '(file)))))

;; -------------------------------------------------------------------
;; Copy & Paste
;; -------------------------------------------------------------------

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
(use-package auto-complete)

;; -------------------------------------------------------------------
;; Fill Column Indicator
;; -------------------------------------------------------------------
(use-package fill-column-indicator
  :init
  (setq fci-rule-color "#f8f8f8") ;;#cccccc
  (define-globalized-minor-mode
    global-fci-mode fci-mode (lambda () (fci-mode 1))))

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

  (setq rm-blacklist
        (mapconcat
         'identity
         ;; These names must start with a space!
         '(" GitGutter" " MRev" " company"
           " Undo-Tree" " Projectile.*" " Z" " Ind"
           " Org-Agenda.*" " ElDoc" " SP/s" " cider.*")
         "\\|")))


;; -------------------------------------------------------------------
;; Auto-saving changed files
;; -------------------------------------------------------------------
(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; -------------------------------------------------------------------
;; Insert Pairs of Matching Elements
;; -------------------------------------------------------------------
;; Highlight parens
(use-package paren
  :config
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "#def")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  ;; (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (setq show-paren-style 'mixed)	;; The entire expression
  (setq blink-matching-paren t)
  (show-paren-mode 1))

;; -------------------------------------------------------------------
;; Save history during sessions
;; -------------------------------------------------------------------
(use-package savehist
  :config
  (savehist-mode t)
  (setq savehist-additional-variables '(extended-command-history kill-ring)))

;; -------------------------------------------------------------------
;; Ripgrep integration
;; -------------------------------------------------------------------
(use-package rg
  :ensure-system-package (rg . ripgrep)
  :init
  (rg-enable-menu))

;; -------------------------------------------------------------------
;; Org-mode
;; -------------------------------------------------------------------
(use-package setup-org-mode
  :load-path local-load-path)

;; -------------------------------------------------------------------
;; Projectile mode
;; -------------------------------------------------------------------
(use-package projectile
  :diminish projectile-mode
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/workspace")
    (setq projectile-project-search-path '("~/workspace")))
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (setq projectile-file-exists-remote-cache-expire nil
        projectile-mode-line nil
        projectile-globally-ignored-directories
        (quote
         (".idea" ".eunit" ".git" ".hg" ".svn"
          ".fslckout" ".bzr" "_darcs" ".tox"
          "build" "target" "_build" ".history"
          "tmp", "*ccls-cache" ".ccls-root"
          ".ccls-cache" "compile_commands.json"
          ".clangd"))
        projectile-require-project-root nil
        projectile-indexing-method 'alien
        ;; projectile-enable-caching nil
        projectile-completion-system 'default
        projectile-svn-command "find . -type f -not -iwholename '*.svn/*' -print0")
  ;; enable projectile mode
  (projectile-mode t))

(use-package counsel-projectile
  :after projectile
  :config
  (setq counsel-projectile-sort-files t)
  (counsel-projectile-mode t))

;; -------------------------------------------------------------------
;; highlight symbol and replace
;; -------------------------------------------------------------------
(use-package highlight-symbol
  :bind* (
          ("C-c h" . highlight-symbol)
          ("C-c r" . highlight-symbol-query-replace)
          ))

;; -------------------------------------------------------------------
;; Enable lsp and treemacs
;; -------------------------------------------------------------------
(defun ff-lsp-mode-setup ()
  "Setup-hook for lsp-mode."
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; Disables lsp linter as default for python-mode. It is crucial that
;; this happens before loading lsp-mode.
(setq lsp-diagnostic-package :none)

(use-package lsp-mode
  :commands lsp
  :hook ((lsp-mode . ff-lsp-mode-setup)
         (c++-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (json-mode . lsp-deferred)
         (yaml-mode . lsp-deferred)
         (before-save-hook . lsp-format-buffer)
         )
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-log-io nil ;; if set to true can cause a performance hit
        lsp-pyls-plugins-flake8-enabled t
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-enable-snippet nil
        lsp-prefer-flymake nil
        lsp-file-watch-threshold 100000 ;; increase watch threshold
        lsp-python-ms-python-executable "/usr/bin/python3"
        )
  (lsp-enable-which-key-integration)

  :bind (:map lsp-mode-map
              ("TAB" . company-indent-or-complete-common)))

;; increase threshold for lsp to run smoothly
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; lsp mode adds automatically capf as first element in the list of
;; company-backends. This disables implicitly the other backends that
;; I define in the specific modes. Hence, each mode takes over
;; responsibility for loading the required completion providers.
(setq lsp-completion-provider :none)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq imenu-auto-rescan t
        imenu-auto-rescan-maxout (* 1024 1024)
        imenu--rescan-item '("" . -99))
  (setq lsp-ui-doc-position 'top
        lsp-ui-doc-alignment 'window
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-doc-include-signature nil  ; don't include type signature in the child frame
        lsp-ui-sideline-show-symbol nil)  ; don't show symbol on the right of info
  (lsp-ui-peek-enable t))

(with-eval-after-load 'lsp-mode
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace))

(with-eval-after-load 'lsp-ui-mode
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package treemacs
  :commands (treemacs
             treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode)
  :bind (("<f8>" . treemacs))
  :init
  (when window-system
    (setq treemacs-width 35
          treemacs-indentation 1
          treemacs-space-between-root-nodes nil)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode nil))
  :config
  ;; (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
  (defun treemacs-custom-filter (file _)
    (or (s-ends-with? ".aux" file)
        (s-ends-with? ".lint" file)
        ))
  (push #'treemacs-custom-filter treemacs-ignored-file-predicates))

(use-package lsp-treemacs
  :after lsp treemacs company
  :commands lsp-treemacs-errors-list
  :config
  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        treemacs-space-between-root-nodes nil
        lsp-idle-delay 0.1 ;; clangd is fast
        ;; be more ide-ish
        lsp-headerline-breadcrumb-enable)
  ;; enables bidirectional sync
  (lsp-treemacs-sync-mode t))

(global-set-key [f9] 'ff/lsp-treemacs-symbols-toggle)

;; -------------------------------------------------------------------
;; Enable dap
;; -------------------------------------------------------------------
(use-package dap-mode
  :init
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1)
  :config (dap-auto-configure-mode)
  :hook ((dap-stopped . (lambda (arg) (call-interactively #'dap-hydra))))
  :bind(("C-c d d" . dap-debug)
        ("C-c d h" . dap-hydra)
        ("C-c d t" . dap-breakpoint-toggle)
        ("C-c d r" . dap-ui-repl)
        ))

;; -------------------------------------------------------------------
;; Eshell & Vterm
;; -------------------------------------------------------------------

(use-package setup-eshell
  :load-path local-load-path)

(use-package setup-vterm
  :load-path local-load-path)

;; -------------------------------------------------------------------
;; Show number of lines in the left side of the buffer
;; -------------------------------------------------------------------
(column-number-mode 1)
(global-display-line-numbers-mode 1)

(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                treemacs-mode-hook
                compilation-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; enable rainbow delimiters for all programming-modes (prog-mode)
(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

;; -------------------------------------------------------------------
;; Drag stuff around with M-up/down
;; -------------------------------------------------------------------
(use-package drag-stuff
  :config
  (drag-stuff-global-mode)
  :bind (
         ("M-<right>" . drag-stuff-right)
         ("M-<left>" . drag-stuff-left)
         ("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)
         ))

;; -------------------------------------------------------------------
;; Yasnippet
;; -------------------------------------------------------------------
(use-package yasnippet
  :config
  (setq yas-verbosity 1
	    yas-wrap-around-region t)

  ;; enable once you use personal snippets
  ;; (with-eval-after-load 'yasnippet
  ;;   (setq yas-snippet-dirs (list (concat emacs-d-custom "/personal-snippets"))))

  (yas-reload-all)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

;; ===================================================================
;; Adjusting modes for programming
;; ===================================================================
;; -------------------------------------------------------------------
;; C/C++
;; -------------------------------------------------------------------
(use-package setup-cc
  :load-path local-load-path)

;; -------------------------------------------------------------------
;; CRAN R
;; -------------------------------------------------------------------
(use-package ess
  :init
  (setq ess-use-eldoc nil)
  ;; ESS will not print the evaluated commands, also speeds up the evaluation
  (setq ess-eval-visibly nil)
  ;; if you don't want to be prompted each time you start an interactive R session
  (setq ess-ask-for-ess-directory nil))

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
  :hook ((LaTeX-mode . TeX-fold-mode)
         (LaTeX-mode . outline-minor-mode)))

(use-package company-auctex
  :after (company auctex))

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
  :hook ((reftex-mode . imenu-add-menubar-index)
         (LaTeX-mode . turn-on-reftex)
         (latex-mode . turn-on-reftex)
         (LaTeX-mode . reftex-mode)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . TeX-PDF-mode)
         ))

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
             (?n . "\\nocite{%l}")))))

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
  :ensure-system-package (okular . okular)
  :bind (:map LaTeX-mode-map
              ("C-c C-a" . okular-jump-to-line)
              :map tex-mode-map
              ("C-c C-a" . okular-jump-to-line))
  :init
  (setq TeX-view-program-list '(("Okular" "okular --unique %o")))
  (setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular")))

  ;; Inverse search
  ;; http://inthearmchair.wordpress.com/2010/09/02/latex-inverse-pdf-search-with-emacs/
  ;; (setq TeX-source-specials-mode 1)         ;; Inverse search

  (setq TeX-auto-global (concat emacs-config-home "/auctex-auto-generated-info/"))
  (setq TeX-auto-local  (concat emacs-config-home "/auctex-auto-generated-info/")))

;; -------------------------------------------------------------------
;; Language tool
;; -------------------------------------------------------------------
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
  (setq langtool-version "5.4"
        langtool-name (concat "LanguageTool-" langtool-version)
        langtool-url (concat "https://languagetool.org/download/" langtool-name ".zip")
        langtool-extract-to (expand-file-name "~/opt/languageTool")
        langtool-expected-binary (concat langtool-extract-to "/" langtool-name "/languagetool-commandline.jar"))
  (ff/download-and-extract-zip-archive langtool-url
                                       langtool-name
                                       langtool-extract-to
                                       langtool-expected-binary
                                       "langtool")
  :config
  (setq langtool-autoshow-message-function 'langtool-autoshow-detail-popup)
  (setq langtool-language-tool-jar langtool-expected-binary)
  :bind (("C-x 4 w" . langtool-check-buffer)
         ("C-x 4 W" . langtool-check-done)
         ("C-x 4 n" . langtool-goto-next-error)
         ("C-x 4 p" . langtool-goto-previous-error)
         ("C-x 4 4" . langtool-show-message-at-point)))

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
    (message "[langtool] Dictionary switched to %s" change)))

(use-package ispell
  :ensure-system-package (("/usr/bin/ispell" . ispell)
                          ("/usr/lib/ispell/american-insane.hash" . iamerican-insane)
                          ("/usr/lib/ispell/ngerman.hash" . ingerman))
  :init
  (setq ispell-dictionary "en_US")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-default-dictionary "en_US")
  (setq flyspell-default-dictionary "en_US")

  (setq ispell-program-name "/usr/bin/aspell")
  (setq ispell-list-command "list")
  (setq ispell-extra-args '("--dont-tex-check-comments"))
  (setq ispell-current-dictionary "en_US")
  :bind (("<f4>" . fd-switch-dictionary)))

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
(use-package setup-python
  :load-path local-load-path)

;; -------------------------------------------------------------------
;; Sphinx documentation
;; -------------------------------------------------------------------
;; docu https://github.com/naiquevin/sphinx-doc.el
;; Use C-c M-d to include doc string in python
(use-package sphinx-doc
  :hook ((python-mode . sphinx-doc-mode))
  :config
  (sphinx-doc-mode t))

;; -------------------------------------------------------------------
;; code style checker
;; -------------------------------------------------------------------
(use-package flycheck
  :diminish flycheck-mode
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (global-flycheck-mode))

;; -------------------------------------------------------------------
;; Shell
;; -------------------------------------------------------------------
(use-package flymake-shell
  :commands flymake-shell-load
  :hook ((sh-set-shell . flymake-shell-load)))

(use-package flymake-shellcheck
  :ensure-system-package (shellcheck . shellcheck)
  :commands flymake-shellcheck-load
  :hook ((sh-mode . flymake-shellcheck-load)))

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
  (compile compile-command))

(use-package swig-mode
  :load-path local-load-path
  :mode (("\\.i$" . swig-mode)))

;; -------------------------------------------------------------------
;; Git - magit
;; -------------------------------------------------------------------
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind (("C-c m h" . magit-log-buffer-file))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  ; Show word based diff
  (setq magit-diff-refine-hunk 'all))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :after magit)

(use-package git-timemachine)

(use-package git-gutter-fringe)

(use-package git-gutter
  :after git-gutter-fringe
  :diminish
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 2)
  (set-face-foreground 'git-gutter-fr:added "LightGreen")
  (fringe-helper-define 'git-gutter-fr:added nil
                        "XXXXXXXXXX"
                        "XXXXXXXXXX"
                        "XXXXXXXXXX"
                        ".........."
                        ".........."
                        "XXXXXXXXXX"
                        "XXXXXXXXXX"
                        "XXXXXXXXXX"
                        ".........."
                        ".........."
                        "XXXXXXXXXX"
                        "XXXXXXXXXX"
                        "XXXXXXXXXX")

  (set-face-foreground 'git-gutter-fr:modified "LightGoldenrod")
  (fringe-helper-define 'git-gutter-fr:modified nil
                        "XXXXXXXXXX"
                        "XXXXXXXXXX"
                        "XXXXXXXXXX"
                        ".........."
                        ".........."
                        "XXXXXXXXXX"
                        "XXXXXXXXXX"
                        "XXXXXXXXXX"
                        ".........."
                        ".........."
                        "XXXXXXXXXX"
                        "XXXXXXXXXX"
                        "XXXXXXXXXX")

  (set-face-foreground 'git-gutter-fr:deleted "LightCoral")
  (fringe-helper-define 'git-gutter-fr:deleted nil
                        "XXXXXXXXXX"
                        "XXXXXXXXXX"
                        "XXXXXXXXXX"
                        ".........."
                        ".........."
                        "XXXXXXXXXX"
                        "XXXXXXXXXX"
                        "XXXXXXXXXX"
                        ".........."
                        ".........."
                        "XXXXXXXXXX"
                        "XXXXXXXXXX"
                        "XXXXXXXXXX")

  ;; These characters are used in terminal mode
  (setq git-gutter:modified-sign "≡")
  (setq git-gutter:added-sign "≡")
  (setq git-gutter:deleted-sign "≡")
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))

;; -------------------------------------------------------------------
;; yaml mode
;; -------------------------------------------------------------------
(use-package yaml-mode
  :ensure-system-package ((pip3 . python3-pip)
                          ("~/.local/lib/python3.6/site-packages/yamllint" . "python3 -m pip install -U 'yamllint'"))
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

(use-package flymake-yaml)

;; -------------------------------------------------------------------
;; dockerfile mode
;; -------------------------------------------------------------------
(use-package dockerfile-mode
  :mode (("Dockerfile\\'" . dockerfile-mode))
  :init
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))

(use-package lsp-docker)

(use-package docker
  :bind ("C-x d" . docker))

;; -------------------------------------------------------------------
;; Kubernetes
;; -------------------------------------------------------------------
(use-package kubernetes)

;; -------------------------------------------------------------------
;; markdown mode
;; -------------------------------------------------------------------
(use-package markdown-mode
  :commands markdown-mode
  :ensure-system-package ((markdown . markdown)
                          (pandoc . pandoc))
  :init
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  :config
  ;; The default command for markdown (~markdown~), doesn't support tables
  ;; (e.g. GitHub flavored markdown). Pandoc does, so let's use that.
  (setq markdown-command "pandoc --from markdown --to html")
  (setq markdown-command-needs-filename t))

;; -------------------------------------------------------------------
;; RST mode
;; -------------------------------------------------------------------
(use-package poly-rst
  :mode (("\\.rst$" . poly-rst-mode)
         ("\\.rest$" . poly-rst-mode))
  :init
  (set-default 'truncate-lines t))

;; C-c C-e r r (org-rst-export-to-rst)
;;    Export as a text file written in reStructured syntax.
;; C-c C-e r R (org-rst-export-as-rst)
;;    Export as a temporary buffer. Do not create a file.
(use-package ox-rst)

(use-package rst
  :mode (("\\.txtt$" . rst-mode)
         ("\\.rst$" . rst-mode)
         ("\\.rest$" . rst-mode))
  ;; enable support of virtualenvironments
  :hook ((rst-mode . pyvenv-mode)))

;; -------------------------------------------------------------------
;; Typescript
;; -------------------------------------------------------------------
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package nvm
  :defer t)

(use-package typescript-mode
  :after (dap-node company)
  :mode (("\\.ts$" . typescript-mode)
         ("\\.tsx$" . typescript-mode))
  :config
  (setq typescript-indent-level 4)
  (dap-node-setup))

;; note, for some resion the mode directive does not work here, so I
;; added the typescript mode explicitly to tsx files.
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(use-package json-snatcher
  :hook ((js-mode-hook . js-mode-bindings)
         (js2-mode-hook . js-mode-bindings))
  :bind (("C-C C-g" . jsons-print-path)))

;; -------------------------------------------------------------------
;; Org + Plantuml mode
;; -------------------------------------------------------------------
(use-package plantuml-mode
  :ensure-system-package (dot . graphviz)
  :mode (("\\.puml" . plantuml-mode)
         ("\\.iuml" . plantuml-mode)
         ("\\.uml" . plantuml-mode))
  :init
  ;; Consider using (plantuml-download-jar) as alternative
  (setq plantuml-version "1.2021.8"
        plantuml-name (concat "plantuml-jar-asl-" plantuml-version)
        plantuml-url (concat "https://sourceforge.net/projects/plantuml/files/" plantuml-version "/" plantuml-name ".zip/download")
        plantuml-extract-to (expand-file-name (concat "~/opt/plantuml/" plantuml-name))
        plantuml-expected-binary (concat plantuml-extract-to "/plantuml.jar"))
  (ff/download-and-extract-zip-archive plantuml-url
                                       plantuml-name
                                       plantuml-extract-to
                                       plantuml-expected-binary
                                       "plantuml")
  :config
  ;; Sample jar configuration
  (setq plantuml-jar-path plantuml-expected-binary
        plantuml-default-exec-mode 'jar
        plantuml-indent-level 4)
  ;; remap preview to personal function; for some reason, bind does
  ;; not accept it
  (define-key plantuml-mode-map [remap plantuml-preview] 'ff/plantum-preview)
  :bind (:map plantuml-mode-map
              ("C-M-i" . plantuml-complete-symbol)))


(use-package flycheck-plantuml
  :after (plantuml-mode flycheck)
  :commands (flycheck-plantuml-setup))

;; -------------------------------------------------------------------
;; Groovy mode for Jenkins
;; -------------------------------------------------------------------
(use-package groovy-mode
  :mode (("\\.groovy$" . groovy-mode)))

(use-package groovy-imports)

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
  :mode (("\\.json$" . json-mode)))

(use-package flymake-json
  :ensure-system-package (jsonlint . "sudo env \"PATH=$PATH\" npm install jsonlint -g")
  :after json-mode
  :hook ((json-mode . flymake-json-load)
         (js-mode . flymake-json-maybe-load)))

;; -------------------------------------------------------------------
;; TOML
;; -------------------------------------------------------------------
(use-package toml-mode
  :mode ("\\.toml\\'" . toml-mode))

;; -------------------------------------------------------------------
;; Jinja2 mode for code generation
;; -------------------------------------------------------------------
(use-package jinja2-mode
  :mode ("\\.j2\\'" "\\.jinja2\\'"))

;; -------------------------------------------------------------------
;; Restclient
;; -------------------------------------------------------------------
(use-package restclient)

;;; init.el ends here
