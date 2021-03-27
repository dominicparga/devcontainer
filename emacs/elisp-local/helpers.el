;;; package --- Set up helper functions
;;; Commentary:
;;; Provides simple helper functions

;;; Code:

(defun ff/download-and-extract-zip-archive (url name extract-to expected-binary-file package-name)
  "Download and install zip archives."
  (let* ((temporary-file (concat temporary-file-directory name ".zip")))
    (unless (file-directory-p extract-to) (make-directory extract-to))
    (unless  (file-exists-p expected-binary-file)
      (unless (file-exists-p temporary-file)
        (message (concat "[" package-name "] Downloading " name))
        (url-copy-file url temporary-file))
      (message (concat "[" package-name "] Decompress " name))
      (call-process-shell-command (concat "unzip " temporary-file " -d " extract-to) nil 0)
      )
    ))

(defun ff/lsp-treemacs-symbols-toggle ()
  "Toggle the lsp-treemacs-symbols buffer."
  (interactive)
  (if (get-buffer "*LSP Symbols List*")
      (kill-buffer "*LSP Symbols List*")
    (progn (lsp-treemacs-symbols)
           (other-window -1))))

(defun ff/python-interpreter-version (type)
  "Provide version of python interpreter."
  (let* ((python-command (if (boundp 'python-shell-interpreter)
                             (eval python-shell-interpreter)
                           "python"))
         (python-interpreter-versions
          (split-string (car (cdr
                              (split-string
                               (shell-command-to-string
                                (concat (eval python-command)
                                        " --version"))
                               " "))) "\\.")))
    (cond ((equal (eval type) "major") (elt python-interpreter-versions 0))
          ((equal (eval type) "minor") (elt python-interpreter-versions 1)))
    ))

(defun ff/python-local-site-packages-path (package-name)
  "Provide the path to the local site packages."
  (concat (expand-file-name "~/.local/lib/python")
          (ff/python-interpreter-version "major")
          "."
          (ff/python-interpreter-version "minor")
          "/site-packages/" (eval package-name))
  )

(provide 'helpers)

;;; helpers.el ends here
