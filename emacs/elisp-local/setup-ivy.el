;;; package --- Set up Ivy, Counsel and Swiper
;;; Commentary:
;;; Sets up ivy for Emacs

;;; Code:

(use-package ivy
  :after (counsel counsel-projectile)
  :bind (("C-s" . swiper)
         ("C-c C-o" . ivy-occur)
         :map ivy-minibuffer-map
         ("RET" . ivy-immediate-done)
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-partial)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  ;; enable ivy mode
  (ivy-mode t)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7)
)

(use-package ivy-hydra)

(use-package ivy-rich
  :init
  ;; Do not enable ivy rich mode here but in all-the-icons-ivy-rich
  ;; (ivy-rich-mode t)
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  ;; needs to be set otherwise (ivy-rich-set-display-transformer) does not get called
  (setq ivy-rich--original-display-transformers-list nil)
  )

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-M-l" . counsel-imenu)
         ("M-y" . counsel-yank-pop)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :init
  ;; enable counsel mode
  (counsel-mode 1)
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil) ;; Don't start searches with ^
  )

;; To display icons correctly, you should run M-x
;; all-the-icons-install-fonts to install the necessary fonts.
(use-package all-the-icons-ivy-rich
  :after (ivy-rich counsel-projectile)
  :init
  (all-the-icons-ivy-rich-mode t)
  (ivy-rich-mode t)
  :config
  ;; Whether display the colorful icons.
  ;; It respects `all-the-icons-color-icons'.
  (setq all-the-icons-ivy-rich-color-icon t)

  ;; The icon size
  (setq all-the-icons-ivy-rich-icon-size 1.0)

  ;; Whether support project root
  (setq all-the-icons-ivy-rich-project t)

  ;; Slow Rendering
  ;; If you experience a slow down in performance when rendering multiple icons simultaneously,
  ;; you can try setting the following variable
  (setq inhibit-compacting-font-caches t)

  ;; (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  ;; (setq inhibit-compacting-font-caches t)
  )

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :init
  (setq ivy-flx-limit 10000))

(use-package smex ;; Adds M-x recent command sorting for counsel-M-x
  :after counsel)

(use-package prescient)

(use-package ivy-prescient
  :after prescient
  :custom
  (ivy-prescient-enable-filtering nil)
  :init
  ;; enable ivy prescient mode
  (ivy-prescient-mode t)
  (prescient-persist-mode t))


(use-package lsp-ivy)

(provide 'setup-ivy)

;;; setup-ivy.el ends here
