;;; package --- Set up helper functions
;;; Commentary:
;;; Provides simple helper functions

;;; Code:

(defun download-and-extract-zip-archive (url name extract-to expected-binary-file package-name)
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

(provide 'helpers)

;;; helpers.el ends here
