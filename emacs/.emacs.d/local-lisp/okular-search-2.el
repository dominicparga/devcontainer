;;; (X)Emacs frontend to forward search with kdvi. See the section on
;;; FORWARD SEARCH in the kdvi manual for more information on forward
;;; search, and for an explanation how to use this script. This script
;;; is a modified version of the script "xdvi-search.el" by Stefan
;;; Ulrich, version 2000/03/13. The
;;; modifications were performed by Stefan Kebekus
;;; . Tested with Emacs 20.7.1 and Xemacs 21.4.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301, USA.
;;;
;;; Please report bugs or improvements, etc. via the "Report bug"-Menu
;;; of okular.
;;;

(defvar okular-script "okular"
  "*Name of start script for okular.")

(defun okular-jump-to-line ()
  "Call okular-script to perform a `forward search' for current file and line number.
See contents of okular-script for details.
If AucTeX is used, the value of TeX-master-file is used as filename
for the master .dvi file; else, the return value of okular-master-file-name
is used (which see)."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line 1)
      (let* (;;; current line in file, as found in the documentation
	     ;;; of emacs. Slightly non-intuitive.
	     (current-line (format "%d" (+ 1 (count-lines (point-min) (point)))))
	     ;;; name of the `main' .tex file, which is also used as .dvi basename:
	     (master-file (expand-file-name (if (fboundp 'TeX-master-file)
						(TeX-master-file t)
					      (okular-get-masterfile (okular-master-file-name)))))
	     ;;; .pdf file name:
	     (pdf-file (concat (file-name-sans-extension master-file) ".pdf"))
	     ;;; current source file name.
	     (filename (expand-file-name (buffer-file-name)))
	     ;; set the relative path corresponding to the master file
	     (filename-relative (file-relative-name filename master-file))
	     (filename-relative (substring filename-relative 1 nil))

	     ;; check whether the project is located on a shared folder
	     (shared (string-match "\sc*" filename))
	     ;; (shared nil)
	     )
	;; Select the absolute or the relative path to the file
	(setq myfile (if shared filename-relative filename))
	(start-process "okular"
		       "okular-output" "okular" ;;; src-args
		      ;;; args for -sourceposition:
		       "--unique" (concat "file:" pdf-file "#src:" current-line
					  myfile))
	(message myfile)
	))))


(defun okular-get-masterfile (file)
  "Small helper function for AucTeX compatibility.
Converts the special value t that TeX-master might be set to
into a real file name."
  (if (eq file t)
      (buffer-file-name)
    file))

(defun okular-master-file-name ()
  "Emulate AucTeX's TeX-master-file function.
Partly copied from tex.el's TeX-master-file and TeX-add-local-master."
  (if (boundp 'TeX-master)
      TeX-master
    (let ((master-file (read-file-name "Master file (default this file): ")))
      (if (y-or-n-p "Save info as local variable? ")
	  (progn
	    (goto-char (point-max))
	    (if (re-search-backward "^\\([^\n]+\\)Local Variables:" nil t)
		(let* ((prefix (if (match-beginning 1)
				   (buffer-substring (match-beginning 1) (match-end 1))
				 ""))
		      (start (point)))
		  (re-search-forward (regexp-quote (concat prefix "End:")) nil t)
		  (if (re-search-backward (regexp-quote (concat prefix "TeX-master")) start t)
		      ;;; if TeX-master line exists already, replace it
		      (progn
			(beginning-of-line 1)
			(kill-line 1))
		    (beginning-of-line 1))
		  (insert prefix "TeX-master: " (prin1-to-string master-file) "\n"))
	      (insert "\n%%% Local Variables: "
;;; mode is of little use without AucTeX ...
;;;		      "\n%%% mode: " (substring (symbol-name major-mode) 0 -5)
		      "\n%%% TeX-master: " (prin1-to-string master-file)
		      "\n%%% End: \n"))
	    (save-buffer)
	    (message "(local variables written.)"))
	(message "(nothing written.)"))
      (set (make-local-variable 'TeX-master) master-file))))

(provide 'okular-search-2)
