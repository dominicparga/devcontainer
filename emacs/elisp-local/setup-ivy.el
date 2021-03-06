;;; package --- Set up Ivy, Counsel and Swiper
;;; Commentary:
;;; Sets up ivy for Emacs

;;; Code:

(use-package ivy
  :diminish
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
  (ivy-mode 1)
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
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-hydra
  :after hydra)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-format-function #'ivy-format-function-line)
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
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :init
  (setq ivy-flx-limit 10000))

(use-package smex ;; Adds M-x recent command sorting for counsel-M-x
  :after counsel)

(use-package wgrep)

(use-package ack
  :ensure-system-package (ack . ack)
  )

(use-package ag
  :ensure-system-package (ag . silversearcher-ag)
  )

;; (use-package ivy-posframe
;;   :custom
;;   (ivy-posframe-width      115)
;;   (ivy-posframe-min-width  115)
;;   (ivy-posframe-height     11)
;;   (ivy-posframe-min-height 11)
;;   :config
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;;   (setq ivy-posframe-parameters '((parent-frame . nil)
;;                                   (left-fringe . 8)
;;                                   (right-fringe . 8)))
;;   (ivy-posframe-mode 1))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1)
  )

(use-package lsp-ivy
  :after ivy
  :after lsp-mode)

(provide 'setup-ivy)

;;; setup-ivy.el ends here
