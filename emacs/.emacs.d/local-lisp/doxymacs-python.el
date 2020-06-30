;;;
;; You can use this snippet to create YARD comments in Ruby buffers and
;; highlight them to the extent the YARD tags match with the Doxygen ones.
;; Any files opened in this buffer after a ruby file was loaded will retain
;; the settings, but this is still the least intrusive method I could find.
;; Another solution would be extending doxymacs itself with another style.

(eval-when-compile (require 'cl))

(defun doxymacs-find-previous-func ()
  "Returns a list describing next function declaration, or nil if not found.

(cdr (assoc 'func (doxymacs-find-next-func))) is the function name (string).
(cdr (assoc 'args (doxymacs-find-next-func))) is a list of arguments.
(cdr (assoc 'return (doxymacs-find-next-func))) is the return type (string).

The argument list is a list of strings."
  (interactive)
  (save-excursion
    (if (re-search-backward
	 (concat
	  ;; return type
	  "\\(\\(const[ \t\n]+\\)?[a-zA-Z0-9_]+[ \t\n*&]+\\)?"

	  ;; name
	  "\\(\\([a-zA-Z0-9_~:<,>*&]\\|\\([ \t\n]+::[ \t\n]+\\)\\)+"
	  "\\(o?perator[ \t\n]*.[^(]*\\)?\\)[ \t\n]*("
	  ) nil t)

	(let* ((func (buffer-substring (match-beginning 3) (match-end 3)))
	       (args (buffer-substring (point) (progn
                                                (backward-char 1)
                                                (forward-list)
                                                (backward-char 1)
                                                (point))))
	       (ret (cond
		     ;; Return type specified
		     ((match-beginning 1)
		      (buffer-substring (match-beginning 1) (match-end 1)))
		     ;;Constructor/destructor
		     ((string-match
		       "^\\([a-zA-Z0-9_<,>:*&]+\\)[ \t\n]*::[ \t\n]*~?\\1$"
		       func) "void")
		     ;;Constructor in class decl.
		     ((save-match-data
			(re-search-backward
			 (concat
			  "class[ \t\n]+" (regexp-quote func) "[ \t\n]*{")
			 nil t))
		      "void")
		     ;;Destructor in class decl.
		     ((save-match-data
			(and (string-match "^~\\([a-zA-Z0-9_]+\\)$" func)
			     (save-match-data
			       (re-search-backward
				(concat
				 "class[ \t\n]+" (regexp-quote
						  (match-string 1 func))
				 "[ \t\n]*{") nil t))))
		      "void")
		     ;;Default
		     (t "int"))))
      (setq args (substring args 2 (length args)))
      ;; (message "%s" args)
      ;; f(a, b, c => backward
      ;; a, b, c   => forward
	  (list (cons 'func func)
		(cons 'args (doxymacs-extract-args-list args))
		(cons 'return (doxymacs-core-string ret))))
    nil)))



(defun doxymacs-python ()
  (doxymacs-mode)
  (font-lock-add-keywords 'ruby-mode doxymacs-doxygen-keywords)
  ;; Only in effect for this buffer
  (make-variable-buffer-local 'doxymacs-file-comment-template)
  (make-variable-buffer-local 'doxymacs-blank-multiline-comment-template)
  (make-variable-buffer-local 'doxymacs-blank-singleline-comment-template)
  (make-variable-buffer-local 'doxymacs-function-comment-template)
  (make-variable-buffer-local 'doxymacs-member-comment-start)
  (make-variable-buffer-local 'doxymacs-member-comment-end)
  (make-variable-buffer-local 'doxymacs-group-comment-start)
  (make-variable-buffer-local 'doxymacs-group-comment-end)
  (make-variable-buffer-local 'doxymacs-parm-tempo-element)

  ;; The templates
  (setq doxymacs-file-comment-template '(
         "#!/usr/bin/python" > n
         "# Copyright (C) 2013 Technische Universitaet Muenchen" > n
         "# This file is part of the SG++ project. For conditions of distribution and" > n
         "# use, please see the copyright notice at http://www5.in.tum.de/SGpp" > n
         "# " > n
         "\"\"\"" '> 'n '> 'p

         "" (doxymacs-doxygen-command-char) "file    "
         (if (buffer-file-name)
             (file-name-nondirectory (buffer-file-name))
           "") > n
           "" (doxymacs-doxygen-command-char) "author  " (user-full-name)
           (doxymacs-user-mail-address)
           > n
           "" (doxymacs-doxygen-command-char) "date    " (current-time-string) > n
           "" > n
           "" (doxymacs-doxygen-command-char) "brief   " (p "Brief description of this file: ") > n
           "" > n
           "" (doxymacs-doxygen-command-char) "version  0.1" > n
           "" p > n
           "\"\"\"" > n
           ))

  (setq doxymacs-blank-multiline-comment-template  '("#" > n "# " p > n "#" > n))
  (setq doxymacs-blank-singleline-comment-template '("# " > p))

  ;; (setq doxymacs-function-comment-template
  ;;       '((let ((next-func (doxymacs-find-next-func)))
  ;;           (if next-func
  ;;               (list
  ;;                'l
  ;;                "# ------------------------------------------------------------" '> 'n
  ;;                "# " 'p '> 'n
  ;;                "#" '> 'n
  ;;                (doxymacs-parm-tempo-element (cdr (assoc 'args next-func)))
  ;;                (unless (string-match
  ;;                         (regexp-quote (cdr (assoc 'return next-func)))
  ;;                         doxymacs-void-types)
  ;;                  '(l "#" > n "# " (doxymacs-doxygen-command-char)
  ;;                      "return " (p "Returns: ") > n))
  ;;                "# ------------------------------------------------------------" '>)
  ;;             (progn
  ;;               (error "Can't find next function declaraton.")
  ;;               nil)))))

  ;; ;; Called when inserting function comments
  ;; (defun doxymacs-parm-tempo-element (parms)
  ;;   (if parms
  ;;       (let ((prompt (concat "Parameter " (car parms) ": ")))
  ;;         (list 'l " " (doxymacs-doxygen-command-char)
  ;;               "param " (car parms) " " (list 'p prompt) '> 'n
  ;;               (doxymacs-parm-tempo-element (cdr parms))))
  ;;     nil))


  (setq doxymacs-function-comment-template
        '((let ((next-func (doxymacs-find-previous-func)))
            (if next-func
                (let ((args (cdr (assoc 'args next-func))))
                  (progn
                    (interactive)
                    (message (if (eq args nil) "TRUE" "FALSE"))
                    (list
                     'l
                     "\"\"\"" '> 'n '> 'p
                     'p '> 'n
                     (if (not (eq args nil))
                         (progn
                           (list
                            'l
                            'p '> 'n
                            "Arguments:" '> 'n
                            (doxymacs-parm-tempo-element args)))
                       nil)
                     (unless (string-match
                              (regexp-quote (cdr (assoc 'return next-func)))
                              doxymacs-void-types)
                       '(l "" > n "Return " (p "Returns: ") > n))
                     'p '> "\"\"\"")))
              (progn
                (error "Can't find next function declaraton.")
                nil)))))


  ;; Called when inserting function comments
  (defun doxymacs-parm-tempo-element (parms)
    (if parms
        (let ((prompt (concat "Parameter " (car parms) ": ")))
          (if (or (string-match "self" prompt)
                  (string-match "args" prompt)
                  (string-match "kws" prompt))
              (doxymacs-parm-tempo-element (cdr parms))
            (list 'l "" (car parms) " -- " (list 'p prompt) '> 'n
                  (doxymacs-parm-tempo-element (cdr parms)))))
      nil))

  ;;
  ;; (defun doxymacs-parm-filter-element (parms)
  ;;   ;; (progn
  ;;   ;;   (setq a (mapcar
  ;;   ;;            (lambda (x)
  ;;   ;;              (unless (or (string-match "self" x)
  ;;   ;;                          (string-match "args" x)
  ;;   ;;                          (string-match "kws" x)) x)) params))
  ;;   ;;   (delq nil a)))

  (setq doxymacs-member-comment-start '("# "))
  (setq doxymacs-member-comment-end '(""))

  (setq doxymacs-group-comment-start '("\"\"\""))
  (setq doxymacs-group-comment-end '("\"\"\""))
  )

(provide 'doxymacs-python)