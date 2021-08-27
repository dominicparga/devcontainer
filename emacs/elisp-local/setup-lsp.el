;;; package --- Set up lsp-mode
;;; Commentary:
;;; Sets up lsp-mode for Emacs

;;; Code:

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

(provide 'setup-lsp)

;;; setup-lsp.el ends here
