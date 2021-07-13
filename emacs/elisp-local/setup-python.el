;;; setup-python.el --- Python setup

;;; Commentary:
;; all the configuration for Python projects

;;; Code:

;; pdb debugger
(defun annotate-pdb ()
  "Colors the background if pdb is active."
  (interactive)
  (highlight-lines-matching-regexp "import ipdb")
  (highlight-lines-matching-regexp "ipdb.set_trace()")
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "pdb.set_trace()"))


(use-package lsp-python-ms
  :ensure-system-package ((pip3 . python3-pip)
                          ("~/.local/lib/python3.6/site-packages/epc" . "python3 -m pip install -U 'epc'")
                          ("~/.local/lib/python3.6/site-packages/ptvsd" . "python3 -m pip install -U 'ptvsd>=4.2'"))
  :mode (
         ("\\.py$" . python-mode)
         ("SConstruct" . python-mode)
         ("SConscript" . python-mode)
         )
  :hook ((python-mode . (lambda ()
                          (setq company-backends +ff/company-default-backends)
                          ))
         (python-mode . annotate-pdb)
         (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          ;; debugging package for python using ptvsd
                          (require 'dap-python)
                          (lsp))))  ; or lsp-deferred
  :init
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "/usr/bin/python3")
  (setq lsp-python-ms-auto-install-server t)
  :config
  ;; delete output buffer on buffer execution
  (setq py-shell-switch-buffers-on-execute nil)
  )

;; install black and black-macchiato with pip3 install --user -U
;; black-macchiato black if black is not available as executable in
;; ~/.local/bin, provide a dummy one that runs black in library mode
;; python3 -m black "${@}"
(use-package python-black
  :ensure-system-package ((black . "python3 -m pip install --user -U black"))
  :hook ((python-mode . python-black-on-save-mode))
  )

;; supports virtual environments. To be set with pyvenv-workon
(use-package pyvenv
  :config
  (pyvenv-mode 1))

(provide 'setup-python)

;;; setup-python.el ends here
