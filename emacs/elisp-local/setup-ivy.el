;;; package --- Set up Ivy, Counsel and Swiper
;;; Commentary:
;;; Sets up ivy for Emacs

;;; Code:

(use-package ivy
  :diminish
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

  ;; enable ivy mode
  (ivy-mode 1)
)

(use-package ivy-hydra
  :after ivy)

(use-package ivy-rich
  :after (counsel counsel-projectile projectile ivy all-the-icons-ivy-rich)
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  ;; needs to be set otherwise (ivy-rich-set-display-transformer) does not get called
  (setq ivy-rich--original-display-transformers-list nil)
  ;; enable ivy rich mode
  (ivy-rich-mode 1)
  )

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-M-l" . counsel-imenu)
         ("M-y" . counsel-yank-pop)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil) ;; Don't start searches with ^

  ;; enable counsel mode
  (counsel-mode 1)
  )

(use-package all-the-icons-ivy-rich
  :after (ivy-rich)
  :init
  (all-the-icons-ivy-rich-mode 1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq inhibit-compacting-font-caches t))

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :init
  (setq ivy-flx-limit 10000))

(use-package smex ;; Adds M-x recent command sorting for counsel-M-x
  :after counsel)

(use-package prescient)

(use-package ivy-prescient
  :after (ivy counsel prescient)
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (prescient-persist-mode t)

  ;; enable ivy prescient mode
  (ivy-prescient-mode t))

(use-package lsp-ivy
  :after (ivy lsp))

(provide 'setup-ivy)

;;; setup-ivy.el ends here
