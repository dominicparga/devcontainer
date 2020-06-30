;; des.el --- major mode for editing and running DES Datalog programs

;; Copyright (C) 2007 Markus Triska <markus.triska@gmx.at>
;; Based on prolog.el by the following authors:

;; Copyright (C) 1986, 1987, 1997, 1998, 1999, 2002, 2003 Free Software Foundation, Inc.

;; Authors: Emil Åström <emil_astrom@hotmail.com>
;;          Milan Zamazal <pdm@freesoft.cz>
;;          Stefan Bruda <bruda@ubishops.ca>     <-- current maintainer
;;          (see below for more details)
;; Keywords: des major mode sicstus swi mercury

;; This mode contains some Prolog leftovers that are not strictly
;; necessary for editing DES programs (like Mercury support etc.).
;; I've left them here to keep the changeset as small as possible.

(defvar des-mode-version "1.14a"
  "DES mode version number")

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Original author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
;; Parts of this file was taken from a modified version of the original
;; by Johan Andersson, Peter Olin, Mats Carlsson, Johan Bevemyr, Stefan
;; Andersson, and Per Danielsson (all SICS people), and Henrik Båkman
;; at Uppsala University, Sweden.
;;
;; Some ideas and also a few lines of code have been borrowed (not stolen ;-)
;; from Oz.el, the Emacs major mode for the Oz programming language,
;; Copyright (C) 1993 DFKI GmbH, Germany, with permission.
;; Authors: Ralf Scheidhauer and Michael Mehl ([scheidhr|mehl]@dfki.uni-sb.de)
;;
;; More ideas and code have been taken from the SICStus debugger mode
;; (http://www.csd.uu.se/~perm/source_debug/index.shtml -- broken link
;; as of Mon May 5 08:23:48 EDT 2003) by Per Mildner.
;;
;; Additions for ECLiPSe and other helpful suggestions: Stephan Heuel
;; <heuel@ipb.uni-bonn.de>

;;; Commentary:
;;
;; This package provides a major mode for editing DES code, with
;; all the bells and whistles one would expect, including syntax
;; highlighting and auto indentation.  It can also send regions to an
;; inferior DES process.
;;
;; The code requires the comint, easymenu, info, imenu, and font-lock
;; libraries. These are normally distributed with GNU Emacs and
;; XEmacs.

;;; Installation:
;;
;; Copy des.el to your load-path and add to your .emacs:
;;
;; (autoload 'des-mode "des" "Major mode for editing DES programs." t)
;; (add-to-list 'auto-mode-alist '("\\.dl$" . des-mode))
;; (setq des-prolog-file "~/des/systems/swi/des.pl")
;;
;;

;; Changelog:

;; Version 1.14:
;;  o  Cleaned up align code. `des-align-flag' is eliminated (since
;;     on a second thought it does not do anything useful).  Added key
;;     binding (C-c C-a) and menu entry for alignment.
;;  o  Condensed regular expressions for lower and upper case
;;     characters (GNU Emacs seems to go over the regexp length limit
;;     with the original form).  My code on the matter was improved
;;     considerably by Markus Triska.
;;  o  Fixed `des-insert-spaces-after-paren' (which used an
;;     unitialized variable).
;;  o  Minor changes to clean up the code and avoid some implicit
;;     package requirements.
;; Version 1.13:
;;  o  Removed the use of `map-char-table' in `des-build-case-strings'
;;     which appears to cause prblems in (at least) Emacs 23.0.0.1.
;;  o  Added if-then-else indentation + corresponding electric
;;     characters.  New customization: `des-electric-if-then-else-flag'
;;  o  Align support (requires `align').  New customization:
;;     `des-align-flag'.
;;  o  Temporary consult files have now the same name throughout the
;;     session.  This prevents issues with reconsulting a buffer
;;     (this event is no longer passed to Des as a request to
;;     consult a new file).
;;  o  Adaptive fill mode is now turned on.  Comment indentation is
;;     still worse than it could be though, I am working on it.
;;  o  Improved filling and auto-filling capabilities.  Now block
;;     comments should be [auto-]filled correctly most of the time;
;;     the following pattern in particular is worth noting as being
;;     filled correctly:
;;         <some code here> % some comment here that goes beyond the
;;                          % rightmost column, possibly combined with
;;                          % subsequent comment lines
;;  o  `des-char-quote-workaround' now defaults to nil.
;;  o  Note: Many of the above improvements have been suggested by
;;     Markus Triska, who also provided useful patches on the matter
;;     when he realized that I was slow in responding.  Many thanks.
;; Version 1.11 / 1.12
;;  o  GNU Emacs compatibility fix for paragraph filling (fixed
;;     incorrectly in 1.11, fix fixed in 1.12).
;; Version 1.10
;;  o  Added paragraph filling in comment blocks and also correct auto
;;     filling for comments.
;;  o  Fixed the possible "Regular expression too big" error in
;;     `des-electric-dot'.
;; Version 1.9
;;  o  Parenthesis expressions are now indented by default so that
;;     components go one underneath the other, just as for compound
;;     terms.  You can use the old style (the second and subsequent
;;     lines being indented to the right in a parenthesis expression)
;;     by setting the customizable variable `des-paren-indent-p'
;;     (group "Des Indentation") to t.
;;  o  (Somehow awkward) handling of the 0' character escape
;;     sequence. I am looking into a better way of doing it but
;;     prospects look bleak.  If this breaks things for you please let
;;     me know and also set the `des-char-quote-workaround' (group
;;     "Des Other") to nil.
;; Version 1.8
;;  o  Key binding fix.
;; Version 1.7
;;  o  Fixed a number of issues with the syntax of single quotes,
;;     including Debian bug #324520.
;; Version 1.6
;;  o  Fixed mercury mode menu initialization (Debian bug #226121).
;;  o  Fixed (i.e., eliminated) Delete remapping (Debian bug #229636).
;;  o  Corrected indentation for clauses defining quoted atoms.
;; Version 1.5:
;;  o  Keywords fontifying should work in console mode so this is
;;     enabled everywhere.
;; Version 1.4:
;;  o  Now supports GNU Des--minor adaptation of a patch by Stefan
;;     Moeding.
;; Version 1.3:
;;  o  Info-follow-nearest-node now called correctly under Emacs too
;;     (thanks to Nicolas Pelletier).  Should be implemented more
;;     elegantly (i.e., without compilation warnings) in the future.
;; Version 1.2:
;;  o  Another prompt fix, still in SWI mode (people seem to have
;;     changed the prompt of SWI Des).
;; Version 1.1:
;;  o  Fixed dots in the end of line comments causing indentation
;;     problems.  The following code is now correctly indented (note
;;     the dot terminating the comment):
;;        a(X) :- b(X),                   
;;            c(X).                  % comment here.
;;        a(X).
;;     and so is this (and variants):
;;        a(X) :- b(X),                   
;;            c(X).                  /* comment here. */
;;        a(X).
;; Version 1.0:
;;  o  Revamped the menu system.
;;  o  Yet another prompt recognition fix (SWI mode).
;;  o  This is more of a renumbering than a new edition.  I promoted
;;     the mode to version 1.0 to emphasize the fact that it is now
;;     mature and stable enough to be considered production (in my
;;     opinion anyway).
;; Version 0.1.41:
;;  o  GNU Emacs compatibility fixes.
;; Version 0.1.40:
;;  o  des-get-predspec is now suitable to be called as
;;     imenu-extract-index-name-function.  The predicate index works.
;;  o  Since imenu works now as advertised, des-imenu-flag is t
;;     by default.
;;  o  Eliminated des-create-predicate-index since the imenu
;;     utilities now work well.  Actually, this function is also
;;     buggy, and I see no reason to fix it since we do not need it
;;     anyway.
;;  o  Fixed des-pred-start, des-clause-start, des-clause-info.
;;  o  Fix for des-build-case-strings; now des-upper-case-string
;;     and des-lower-case-string are correctly initialized, 
;;  o  Various font-lock changes; most importantly, block comments (/*
;;     ... */) are now correctly fontified in XEmacs even when they
;;     extend on multiple lines.
;; Version 0.1.36: 
;;  o  The debug prompt of SWI is now correctly recognized.
;; Version 0.1.35: 
;;  o  Minor font-lock bug fixes.


;;; Code:

(eval-when-compile
  (require 'compile)
  (require 'font-lock)
  ;; We need imenu everywhere because of the predicate index!
  (require 'imenu)
  ;)
  (require 'info)
  (require 'shell)
  )

(require 'comint)
(require 'easymenu)
(require 'align)


(defgroup des nil
  "Major modes for editing and running DES files."
  :group 'languages)

(defgroup des-faces nil
  "DES mode specific faces."
  :group 'font-lock)

(defgroup des-indentation nil
  "DES mode indentation configuration."
  :group 'des)

(defgroup des-font-lock nil
  "DES mode font locking patterns."
  :group 'des)

(defgroup des-keyboard nil
  "DES mode keyboard flags."
  :group 'des)

(defgroup des-inferior nil
  "Inferior DES mode options."
  :group 'des)

(defgroup des-other nil
  "Other DES mode options."
  :group 'des)


;;-------------------------------------------------------------------
;; User configurable variables
;;-------------------------------------------------------------------

;; General configuration

(defcustom des-system nil
  "*Des interpreter/compiler used.
The value of this variable is nil or a symbol.
If it is a symbol, it determines default values of other configuration
variables with respect to properties of the specified Des
interpreter/compiler.

Currently recognized symbol values are:
eclipse - Eclipse Des
mercury - Mercury
sicstus - SICStus Des
swi     - SWI Des
gnu     - GNU Des"
  :group 'des
  :type '(choice (const :tag "SICStus" :value sicstus)
                 (const :tag "SWI Des" :value swi)
                 (const :tag "Default" :value nil)))
(make-variable-buffer-local 'des-system)

;; NB: This alist can not be processed in des-mode-variables to
;; create a des-system-version-i variable since it is needed
;; prior to the call to des-mode-variables.
(defcustom des-system-version
  '((sicstus  (3 . 6))
    (swi      (0 . 0))
    (mercury  (0 . 0))
    (eclipse  (3 . 7))
    (gnu      (0 . 0)))
  "*Alist of Des system versions.
The version numbers are of the format (Major . Minor).")


;; Indentation

(defcustom des-indent-width 2
  "*The indentation width used by the editing buffer."
  :group 'des-indentation
  :type 'integer)  

(defcustom des-align-comments-flag t
  "*Non-nil means automatically align comments when indenting."
  :group 'des-indentation
  :type 'boolean)

(defcustom des-indent-mline-comments-flag t
  "*Non-nil means indent contents of /* */ comments.
Otherwise leave such lines as they are."
  :group 'des-indentation
  :type 'boolean)

(defcustom des-object-end-to-0-flag t
  "*Non-nil means indent closing '}' in SICStus object definitions to level 0.
Otherwise indent to `des-indent-width'."
  :group 'des-indentation
  :type 'boolean)

(defcustom des-left-indent-regexp "\\(;\\|\\*?->\\)"
  "*Regexp for character sequences after which next line is indented.
Next line after such a regexp is indented to the opening paranthesis level."
  :group 'des-indentation
  :type 'regexp)

(defcustom des-paren-indent-p nil
  "*If non-nil, increase indentation for parenthesis expressions.
The second and subsequent line in a parenthesis expression other than
a compound term can either be indented `des-paren-indent' to the
right (if this variable is non-nil) or in the same way as for compound
terms (if this variable is nil, default)."
  :group 'des-indentation
  :type 'boolean)

(defcustom des-paren-indent 4
  "*The indentation increase for parenthesis expressions.
Only used in ( If -> Then ; Else) and ( Disj1 ; Disj2 ) style expressions."
  :group 'des-indentation
  :type 'integer)

(defcustom des-parse-mode 'beg-of-clause
  "*The parse mode used (decides from which point parsing is done).
Legal values:
'beg-of-line   - starts parsing at the beginning of a line, unless the
                 previous line ends with a backslash. Fast, but has
                 problems detecting multiline /* */ comments.
'beg-of-clause - starts parsing at the beginning of the current clause.
                 Slow, but copes better with /* */ comments."
  :group 'des-indentation
  :type '(choice (const :value beg-of-line)
                 (const :value beg-of-clause)))

;; Font locking

(defcustom des-keywords
  '((eclipse
     ("use_module" "begin_module" "module_interface" "dynamic"
      "external" "export" "dbgcomp" "nodbgcomp" "compile"))
    (mercury
     ("all" "else" "end_module" "equality" "external" "fail" "func" "if"
      "implementation" "import_module" "include_module" "inst" "instance"
      "interface" "mode" "module" "not" "pragma" "pred" "some" "then" "true"
      "type" "typeclass" "use_module" "where"))
    (sicstus
     ("block" "dynamic" "mode" "module" "multifile" "meta_predicate"
      "parallel" "public" "sequential" "volatile"))
    (swi
     ("discontiguous" "dynamic" "ensure_loaded" "export" "export_list" "import"
      "meta_predicate" "module" "module_transparent" "multifile" "require"
      "use_module" "volatile"))
    (gnu
     ("built_in" "char_conversion" "discontiguous" "dynamic" "ensure_linked"
      "ensure_loaded" "foreign" "include" "initialization" "multifile" "op"
      "public" "set_des_flag"))
    (t
     ("dynamic" "module")))
  "*Alist of Des keywords which is used for font locking of directives."
  :group 'des-font-lock
  :type 'sexp)

(defcustom des-types
  '((mercury
     ("char" "float" "int" "io__state" "string" "univ"))
    (t nil))
  "*Alist of Des types used by font locking."
  :group 'des-font-lock
  :type 'sexp)

(defcustom des-mode-specificators
  '((mercury
     ("bound" "di" "free" "ground" "in" "mdi" "mui" "muo" "out" "ui" "uo"))
    (t nil))
  "*Alist of Des mode specificators used by font locking."
  :group 'des-font-lock
  :type 'sexp)

(defcustom des-determinism-specificators
  '((mercury
     ("cc_multi" "cc_nondet" "det" "erroneous" "failure" "multi" "nondet"
      "semidet"))
    (t nil))
  "*Alist of Des determinism specificators used by font locking."
  :group 'des-font-lock
  :type 'sexp)

(defcustom des-directives
  '((mercury
     ("^#[0-9]+"))
    (t nil))
  "*Alist of Des source code directives used by font locking."
  :group 'des-font-lock
  :type 'sexp)


;; Keyboard

(defcustom des-electric-newline-flag t
  "*Non-nil means automatically indent the next line when the user types RET."
  :group 'des-keyboard
  :type 'boolean)

(defcustom des-hungry-delete-key-flag nil
  "*Non-nil means delete key consumes all preceding spaces."
  :group 'des-keyboard
  :type 'boolean)

(defcustom des-electric-dot-flag t
  "*Non-nil means make dot key electric.
Electric dot appends newline or inserts head of a new clause.
If dot is pressed at the end of a line where at least one white space
precedes the point, it inserts a recursive call to the current predicate.
If dot is pressed at the beginning of an empty line, it inserts the head
of a new clause for the current predicate. It does not apply in strings
and comments. 
It does not apply in strings and comments."
  :group 'des-keyboard
  :type 'boolean)

(defcustom des-electric-dot-full-predicate-template nil
  "*If nil, electric dot inserts only the current predicate's
name and `(' for recursive calls or new clause heads. Non-nil
means to also insert enough commata to cover the predicate's
arity and `)', and dot and newline for recursive calls."
  :group 'des-keyboard
  :type 'boolean)

(defcustom des-electric-underscore-flag nil
  "*Non-nil means make underscore key electric.
Electric underscore replaces the current variable with underscore.
If underscore is pressed not on a variable then it behaves as usual."
  :group 'des-keyboard
  :type 'boolean)

(defcustom des-electric-tab-flag nil
  "*Non-nil means make TAB key electric.
Electric TAB inserts spaces after parentheses, ->, and ;
in ( If -> Then ; Else) and ( Disj1 ; Disj2 ) style expressions."
  :group 'des-keyboard
  :type 'boolean)

(defcustom des-electric-if-then-else-flag nil
  "*Non-nil makes `(', `>' and `;' electric to automatically
indent if-then-else constructs."
  :group 'des-keyboard
  :type 'boolean)

(defcustom des-electric-colon-flag nil
  "*If non-nil, pressing `:' at the end of a line that starts in
the first column (i.e., clause heads) inserts ` :-' and newline."
  :group 'des-keyboard
  :type 'boolean)

(defcustom des-electric-dash-flag nil
  "*If non-nil, pressing `-' at the end of a line that starts in
the first column (i.e., DCG heads) inserts ` -->' and newline."
  :group 'des-keyboard
  :type 'boolean)

(defcustom des-old-sicstus-keys-flag nil
  "*Non-nil means old SICStus Des mode keybindings are used."
  :group 'des-keyboard
  :type 'boolean)

;; Inferior mode

(defcustom des-program-name
  '(((getenv "EDES") (eval (getenv "EDES")))
    (eclipse "eclipse")
    (mercury nil)
    (sicstus "sicstus")
    (swi "pl")
    (gnu "gdes")
    (t "des"))
  "*Alist of program names for invoking an inferior Des with `run-des'."
  :group 'des-inferior
  :type 'sexp)

(defcustom des-program-switches
  '((sicstus ("-i"))
    (t nil))
  "*Alist of switches given to inferior Des run with `run-des'."
  :group 'des-inferior
  :type 'sexp)

(defcustom des-consult-string
  '((eclipse "[%f].")
    (mercury nil)
    (sicstus (eval (if (des-atleast-version '(3 . 7))
                       "des:zap_file(%m,%b,consult,%l)."
                     "des:zap_file(%m,%b,consult).")))
    (swi "[%f].")
    (gnu     "[%f].")
    (t "reconsult(%f)."))
  "*Alist of strings defining predicate for reconsulting.

Some parts of the string are replaced:
`%f' by the name of the consulted file (can be a temporary file)
`%b' by the file name of the buffer to consult
`%m' by the module name and name of the consulted file separated by colon
`%l' by the line offset into the file. This is 0 unless consulting a
     region of a buffer, in which case it is the number of lines before
     the region."
  :group 'des-inferior
  :type 'sexp)

(defcustom des-compile-string
  '((eclipse "[%f].")
    (mercury "mmake ")
    (sicstus (eval (if (des-atleast-version '(3 . 7))
                       "des:zap_file(%m,%b,compile,%l)."
                     "des:zap_file(%m,%b,compile).")))
    (swi "[%f].")
    (t "compile(%f)."))
  "*Alist of strings and lists defining predicate for recompilation.

Some parts of the string are replaced:
`%f' by the name of the compiled file (can be a temporary file)
`%b' by the file name of the buffer to compile
`%m' by the module name and name of the compiled file separated by colon
`%l' by the line offset into the file. This is 0 unless compiling a
     region of a buffer, in which case it is the number of lines before
     the region.

If `des-program-name' is non-nil, it is a string sent to a Des process.
If `des-program-name' is nil, it is an argument to the `compile' function."
  :group 'des-inferior
  :type 'sexp)

(defcustom des-eof-string "end_of_file.\n"
  "*Alist of strings that represent end of file for des.
nil means send actual operating system end of file."
  :group 'des-inferior
  :type 'sexp)

(defcustom des-prompt-regexp
  '((eclipse "^[a-zA-Z0-9()]* *\\?- \\|^\\[[a-zA-Z]* [0-9]*\\]:")
    (sicstus "| [ ?][- ] *")
    (swi "^\\(\\[[a-zA-Z]*\\] \\)?[1-9]?[0-9]*[ ]?\\?- \\|^| +")
    (t "^ *\\?-"))
  "*Alist of prompts of the des system command line."
  :group 'des-inferior
  :type 'sexp)

(defcustom des-continued-prompt-regexp
  '((sicstus "^\\(| +\\|     +\\)")
    (t "^|: +"))
  "*Alist of regexps matching the prompt when consulting `user'."
  :group 'des-inferior
  :type 'sexp)

(defcustom des-debug-on-string "debug.\n"
  "*Predicate for enabling debug mode."
  :group 'des-inferior
  :type 'string)

(defcustom des-debug-off-string "nodebug.\n"
  "*Predicate for disabling debug mode."
  :group 'des-inferior
  :type 'string)

(defcustom des-trace-on-string "trace.\n"
  "*Predicate for enabling tracing."
  :group 'des-inferior
  :type 'string)

(defcustom des-trace-off-string "notrace.\n"
  "*Predicate for disabling tracing."
  :group 'des-inferior
  :type 'string)

(defcustom des-zip-on-string "zip.\n"
  "*Predicate for enabling zip mode for SICStus."
  :group 'des-inferior
  :type 'string)

(defcustom des-zip-off-string "nozip.\n"
  "*Predicate for disabling zip mode for SICStus."
  :group 'des-inferior
  :type 'string)

(defcustom des-use-standard-consult-compile-method-flag t
  "*Non-nil means use the standard compilation method.
Otherwise the new compilation method will be used. This
utilises a special compilation buffer with the associated
features such as parsing of error messages and automatically
jumping to the source code responsible for the error.

Warning: the new method is so far only experimental and
does contain bugs. The recommended setting for the novice user
is non-nil for this variable."
  :group 'des-inferior
  :type 'boolean)


;; Miscellaneous

(defcustom des-use-des-tokenizer-flag t
  "*Non-nil means use the internal des tokenizer for indentation etc.
Otherwise use `parse-partial-sexp' which is faster but sometimes incorrect."
  :group 'des-other
  :type 'boolean)

(defcustom des-imenu-flag t
  "*Non-nil means add a clause index menu for all des files."
  :group 'des-other
  :type 'boolean)

(defcustom des-imenu-max-lines 3000
  "*The maximum number of lines of the file for imenu to be enabled.
Relevant only when `des-imenu-flag' is non-nil."
  :group 'des-other
  :type 'integer)

(defcustom des-info-predicate-index
  "(sicstus)Predicate Index"
  "*The info node for the SICStus predicate index."
  :group 'des-other
  :type 'string)

(defcustom des-underscore-wordchar-flag t
  "*Non-nil means underscore (_) is a word-constituent character."
  :group 'des-other
  :type 'boolean)

(defcustom des-use-sicstus-sd nil
  "*If non-nil, use the source level debugger of SICStus 3#7 and later."
  :group 'des-other
  :type 'boolean)

(defcustom des-char-quote-workaround nil
  "*If non-nil, declare 0 as a quote character so that 0'<char> does not break syntax highlighting.
This is really kludgy but I have not found any better way of handling it."
  :group 'des-other
  :type 'boolean)


;;-------------------------------------------------------------------
;; Internal variables
;;-------------------------------------------------------------------

(defvar des-emacs 
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      'xemacs
    'gnuemacs)
  "The variant of Emacs we're running.
Valid values are 'gnuemacs and 'xemacs.")

(defvar des-known-systems '(eclipse mercury sicstus swi gnu))

;(defvar des-temp-filename "")   ; Later set by `des-temporary-file'

(defvar des-mode-syntax-table nil)
(defvar des-mode-abbrev-table nil)
(defvar des-mode-map nil)
(defvar des-upper-case-string ""
  "A string containing all upper case characters.
Set by des-build-case-strings.")
(defvar des-lower-case-string ""
  "A string containing all lower case characters.
Set by des-build-case-strings.")
  
(defvar des-atom-char-regexp ""
  "Set by des-set-atom-regexps.")
;; "Regexp specifying characters which constitute atoms without quoting.")
(defconst des-atom-regexp ""
  "Set by des-set-atom-regexps.")

(defconst des-left-paren "[[({]" 
  "The characters used as left parentheses for the indentation code.")
(defconst des-right-paren "[])}]"
  "The characters used as right parentheses for the indentation code.")

(defconst des-quoted-atom-regexp
  "\\(^\\|[^0-9]\\)\\('\\([^\n']\\|\\\\'\\)*'\\)"
  "Regexp matching a quoted atom.")
(defconst des-string-regexp
  "\\(\"\\([^\n\"]\\|\\\\\"\\)*\"\\)"
  "Regexp matching a string.")
(defconst des-head-delimiter "\\(:-\\|\\+:\\|-:\\|\\+\\?\\|-\\?\\|-->\\)"
  "A regexp for matching on the end delimiter of a head (e.g. \":-\").")

(defvar des-compilation-buffer "*des-compilation*"
  "Name of the output buffer for Des compilation/consulting.")

(defvar des-temporary-file-name nil)
(defvar des-keywords-i nil)
(defvar des-types-i nil)
(defvar des-mode-specificators-i nil)
(defvar des-determinism-specificators-i nil)
(defvar des-directives-i nil)
(defvar des-program-name-i nil)
(defvar des-program-switches-i nil)
(defvar des-consult-string-i nil)
(defvar des-compile-string-i nil)
(defvar des-eof-string-i nil)
(defvar des-prompt-regexp-i nil)
(defvar des-continued-prompt-regexp-i nil)
(defvar des-help-function-i nil)

(defvar des-align-rules
  (eval-when-compile
    (mapcar
     (lambda (x)
       (let ((name (car x))
             (sym  (cdr x)))
         `(,(intern (format "des-%s" name))
           (regexp . ,(format "\\(\\s-*\\)%s\\(\\s-*\\)" sym))
           (tab-stop . nil)
           (modes . '(des-mode))
           (group . (1 2)))))
     '(("dcg" . "-->") ("rule" . ":-") ("simplification" . "<=>")
       ("propagation" . "==>")))))



;;-------------------------------------------------------------------
;; Des mode
;;-------------------------------------------------------------------

;; Example: (des-atleast-version '(3 . 6))
(defun des-atleast-version (version)
  "Return t if the version of the current des system is VERSION or later.
VERSION is of the format (Major . Minor)"
  ;; Version.major < major or
  ;; Version.major = major and Version.minor <= minor
  (let* ((thisversion (des-find-value-by-system des-system-version))
         (thismajor (car thisversion))
         (thisminor (cdr thisversion)))
    (or (< (car version) thismajor)
        (and (= (car version) thismajor)
             (<= (cdr version) thisminor)))
    ))

(if des-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (if des-underscore-wordchar-flag
        (modify-syntax-entry ?_ "w" table)
      (modify-syntax-entry ?_ "_" table))

    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\' "\"" table)

    ;; Any better way to handle the 0'<char> construct?!?
    (when des-char-quote-workaround
      (modify-syntax-entry ?0 "\\" table))

    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (if (eq des-emacs 'xemacs)
        (progn
          (modify-syntax-entry ?* ". 67" table)
          (modify-syntax-entry ?/ ". 58" table)
          )
      ;; Emacs wants to see this it seems:
      (modify-syntax-entry ?* ". 23b" table)
      (modify-syntax-entry ?/ ". 14" table)
    )
    (setq des-mode-syntax-table table)))

(define-abbrev-table 'des-mode-abbrev-table ())

(defun des-find-value-by-system (alist)
  "Get value from ALIST according to `des-system'."
  (if (listp alist)
      (let (result
            id)
        (while alist
          (setq id (car (car alist)))
          (if (or (eq id des-system)
                  (eq id t)
                  (and (listp id)
                       (eval id)))
              (progn
                (setq result (car (cdr (car alist))))
                (if (and (listp result)
                         (eq (car result) 'eval))
                    (setq result (eval (car (cdr result)))))
                (setq alist nil))
            (setq alist (cdr alist))))
        result)
    alist))

(defun des-mode-variables ()
  "Set some common variables to Des code specific values."
  (setq local-abbrev-table des-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "[ \t]*$\\|" page-delimiter)) ;'%%..'
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode t)
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'des-do-auto-fill)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'des-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  ;; This complex regexp makes sure that comments cannot start
  ;; inside quoted atoms or strings
  (setq comment-start-skip 
        (format "^\\(\\(%s\\|%s\\|[^\n\'\"%%]\\)*\\)\\(/\\*+ *\\|%%+ *\\)" 
                des-quoted-atom-regexp des-string-regexp))
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'des-comment-indent)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'des-comment-indent)
  (make-local-variable 'parens-require-spaces)
  (setq parens-require-spaces nil)
  ;; Initialize Des system specific variables
  (let ((vars '(des-keywords des-types des-mode-specificators
                des-determinism-specificators des-directives
                des-program-name des-program-switches
                des-consult-string des-compile-string des-eof-string
                des-prompt-regexp des-continued-prompt-regexp
                des-help-function)))
    (while vars
      (set (intern (concat (symbol-name (car vars)) "-i"))
           (des-find-value-by-system (eval (car vars))))
      (setq vars (cdr vars))))
  (when (null des-program-name-i)
        (make-local-variable 'compile-command)
        (setq compile-command des-compile-string-i))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(des-font-lock-keywords nil nil ((?_ . "w"))))
)

(defun des-mode-keybindings-common (map)
  "Define keybindings common to both Des modes in MAP."
  (define-key map "\C-c?" 'des-help-on-predicate)
  (define-key map "\C-c/" 'des-help-apropos))

(defun des-mode-keybindings-edit (map)
  "Define keybindings for Des mode in MAP."
  (define-key map "\M-a" 'des-beginning-of-clause)
  (define-key map "\M-e" 'des-end-of-clause)
  (define-key map "\M-q" 'des-fill-paragraph)
  (define-key map "\C-c\C-a" 'align)
  (define-key map "\C-\M-a" 'des-beginning-of-predicate)
  (define-key map "\C-\M-e" 'des-end-of-predicate)
  (define-key map "\M-\C-c" 'des-mark-clause)
  (define-key map "\M-\C-h" 'des-mark-predicate)
  (define-key map "\M-\C-n" 'des-forward-list)
  (define-key map "\M-\C-p" 'des-backward-list)
  (define-key map "\C-c\C-n" 'des-insert-predicate-template)
  (define-key map "\C-c\C-s" 'des-insert-predspec)
  (define-key map "\M-\r" 'des-insert-next-clause)
  (define-key map "\C-c\C-va" 'des-variables-to-anonymous)
  (define-key map "\C-c\C-v\C-s" 'des-view-predspec)

  (define-key map [Backspace] 'des-electric-delete)
  (define-key map "." 'des-electric-dot)
  (define-key map "_" 'des-electric-underscore)
  (define-key map "(" 'des-electric-if-then-else)
  (define-key map ";" 'des-electric-if-then-else)
  (define-key map ">" 'des-electric-if-then-else)
  (define-key map ":" 'des-electric-colon)
  (define-key map "-" 'des-electric-dash)
  (if des-electric-newline-flag 
      (define-key map "\r" 'newline-and-indent))

  ;; If we're running SICStus, then map C-c C-c e/d to enabling
  ;; and disabling of the source-level debugging facilities.
  ;(if (and (eq des-system 'sicstus)
  ;         (des-atleast-version '(3 . 7)))
  ;    (progn
  ;      (define-key map "\C-c\C-ce" 'des-enable-sicstus-sd)
  ;      (define-key map "\C-c\C-cd" 'des-disable-sicstus-sd)
  ;      ))

    (define-key map "\C-c\C-r" 'des-consult)
    (define-key map "\C-c\C-b" 'des-consult)
    (define-key map "\C-c\C-f" 'des-consult)
    (define-key map [f1] 'des-consult))

(defun des-mode-keybindings-inferior (map)
  "Define keybindings for inferior Des mode in MAP."
  ;; No inferior mode specific keybindings now.
  )

(if des-mode-map
    ()
  (setq des-mode-map (make-sparse-keymap))
  (des-mode-keybindings-common des-mode-map)
  (des-mode-keybindings-edit des-mode-map)
  ;; System dependent keymaps for system dependent menus
  (let ((systems des-known-systems))
    (while systems
      (set (intern (concat "des-mode-map-"
                           (symbol-name (car systems))))
           ;(cons 'keymap des-mode-map)
           des-mode-map
           )
      (setq systems (cdr systems))))
  )
  

(defvar des-mode-hook nil
  "List of functions to call after the des mode has initialised.")

;;;###autoload
(defun des-mode (&optional system)
  "Major mode for editing DES code.

Blank lines and `%%...' separate paragraphs.  `%'s starts a comment
line and comments can also be enclosed in /* ... */.

To find out what version of DES mode you are running, enter
`\\[des-mode-version]'.

Commands:
\\{des-mode-map}
Entry to this mode calls the value of `des-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if system (setq des-system system))
  (use-local-map
   (if des-system 
       ;; ### Looks like it works under XEmacs as well...
       ;; (and des-system
       ;;     (not (eq des-emacs 'xemacs)))
       (eval (intern (concat "des-mode-map-" (symbol-name des-system))))
     des-mode-map)
   )
  (setq major-mode 'des-mode)
  (setq mode-name (concat "DES"
                          (cond
                           ((eq des-system 'eclipse) "[ECLiPSe]")
                           ((eq des-system 'mercury) "[Mercury]")
                           ((eq des-system 'sicstus) "[SICStus]")
                           ((eq des-system 'swi) "[SWI]")
                           ((eq des-system 'gnu) "[GNU]")
                           (t ""))))
  (set-syntax-table des-mode-syntax-table)
  (des-mode-variables)
  (des-build-case-strings)
  (des-set-atom-regexps)
  (dolist (ar des-align-rules) (add-to-list 'align-rules-list ar))

  ;; imenu entry moved to the appropriate hook for consistency
  
  ;; Load SICStus debugger if suitable
  (if (and (eq des-system 'sicstus)
           (des-atleast-version '(3 . 7))
           des-use-sicstus-sd)
      (des-enable-sicstus-sd))
  
  (run-hooks 'des-mode-hook))

;;;###autoload
(defun mercury-mode ()
  "Major mode for editing Mercury programs.
Actually this is just customized `des-mode'."
  (interactive)
  (des-mode 'mercury))


;;-------------------------------------------------------------------
;; Inferior des mode
;;-------------------------------------------------------------------

(defvar des-inferior-mode-map nil)
(defvar des-inferior-mode-hook nil
  "List of functions to call after the inferior des mode has initialised.")

(defun des-inferior-mode ()
  "Major mode for interacting with an inferior Des process.

The following commands are available:
\\{des-inferior-mode-map}

Entry to this mode calls the value of `des-mode-hook' with no arguments,
if that value is non-nil.  Likewise with the value of `comint-mode-hook'.
`des-mode-hook' is called after `comint-mode-hook'.

You can send text to the inferior Des from other buffers
using the commands `send-region', `send-string' and \\[des-consult-region].

Commands:
Tab indents for Des; with argument, shifts rest
 of expression rigidly with the current line.
Paragraphs are separated only by blank lines and '%%'. '%'s start comments.

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
\\[comint-delchar-or-maybe-eof] sends end-of-file as input.
\\[comint-kill-input] and \\[backward-kill-word] are kill commands,
imitating normal Unix input editing.
\\[comint-interrupt-subjob] interrupts the shell or its current subjob if any.
\\[comint-stop-subjob] stops, likewise.
\\[comint-quit-subjob] sends quit signal, likewise.

To find out what version of Des mode you are running, enter
`\\[des-mode-version]'."
  (interactive)
  (cond ((not (eq major-mode 'des-inferior-mode))
         (kill-all-local-variables)
         (comint-mode)
         (setq comint-input-filter 'des-input-filter)
         (setq major-mode 'des-inferior-mode)
         (setq mode-name "Inferior Des")
         (setq mode-line-process '(": %s"))
         (des-mode-variables)
         (if des-inferior-mode-map
             ()
           (setq des-inferior-mode-map (copy-keymap comint-mode-map))
           (des-mode-keybindings-common des-inferior-mode-map)
           (des-mode-keybindings-inferior des-inferior-mode-map))
         (use-local-map des-inferior-mode-map)
         (setq comint-prompt-regexp des-prompt-regexp-i)
         (make-variable-buffer-local 'shell-dirstack-query)
         (setq shell-dirstack-query "pwd.")
         (run-hooks 'des-inferior-mode-hook))))

(defun des-input-filter (str)
  (cond ((string-match "\\`\\s *\\'" str) nil) ;whitespace
        ((not (eq major-mode 'des-inferior-mode)) t)
        ((= (length str) 1) nil)        ;one character
        ((string-match "\\`[rf] *[0-9]*\\'" str) nil) ;r(edo) or f(ail)
        (t t)))

;;;###autoload
(defun run-des (arg)
  "Run an inferior Des process, input and output via buffer *des*.
With prefix argument ARG, restart the Des process if running before."
  (interactive "P")
  (if (and arg (get-process "des"))
      (progn
        (process-send-string "des" "halt.\n")
        (while (get-process "des") (sit-for 0.1))))
  (let ((buff (buffer-name)))
    (if (not (string= buff "*des*"))
        (des-goto-des-process-buffer))
    ;; Load SICStus debugger if suitable
    (if (and (eq des-system 'sicstus)
             (des-atleast-version '(3 . 7))
             des-use-sicstus-sd)
        (des-enable-sicstus-sd))
    (des-mode-variables)
    (des-ensure-process)
    ))

(defun des-ensure-process (&optional wait)
  "If Des process is not running, run it.
If the optional argument WAIT is non-nil, wait for Des prompt specified by
the variable `des-prompt-regexp'."
  (if (null des-program-name-i)
      (error "This Des system has defined no interpreter."))
  (if (comint-check-proc "*des*")
      ()
    (apply 'make-comint "des" des-program-name-i nil
           des-program-switches-i)
    (save-excursion
      (set-buffer "*des*")
      (des-inferior-mode)
      (if wait
          (progn
            (goto-char (point-max))
            (while
                (save-excursion
                  (not
                   (re-search-backward
                    (concat "\\(" des-prompt-regexp-i "\\)" "\\=")
                    nil t)))
              (sit-for 0.1)))))))

(defun des-process-insert-string (process string)
  "Insert STRING into inferior Des buffer running PROCESS."
  ;; Copied from elisp manual, greek to me
  (let ((buf (current-buffer)))
    (unwind-protect
        (let (moving)
          (set-buffer (process-buffer process))
          (setq moving (= (point) (process-mark process)))
          (save-excursion
            ;; Insert the text, moving the process-marker.
            (goto-char (process-mark process))
            (insert string)
            (set-marker (process-mark process) (point)))
          (if moving (goto-char (process-mark process))))
      (set-buffer buf))))


;;------------------------------------------------------------
;; Old consulting and compiling functions
;;------------------------------------------------------------

(defun des-old-process-region (compilep start end)
  "Process the region limited by START and END positions.
If COMPILEP is non-nil then use compilation, otherwise consulting."
   (des-ensure-process)
   ;(let ((tmpfile des-temp-filename)
   (let ((tmpfile (des-bsts (des-temporary-file)))
         ;(process (get-process "des"))
         (first-line (1+ (count-lines 
                          (point-min) 
                          (save-excursion
                            (goto-char start)
                            (point))))))
     (write-region start end tmpfile)
     (process-send-string
      "des" (des-build-des-command
                compilep tmpfile (des-bsts buffer-file-name)
                first-line))
     (des-goto-des-process-buffer)))

(defun des-old-process-predicate (compilep)
  "Process the predicate around point.
If COMPILEP is non-nil then use compilation, otherwise consulting."
  (des-old-process-region
   compilep (des-pred-start) (des-pred-end)))

(defun des-old-process-buffer (compilep)
  "Process the entire buffer.
If COMPILEP is non-nil then use compilation, otherwise consulting."
  (des-old-process-region compilep (point-min) (point-max)))

(defun des-old-process-file (compilep)
  "Process the file of the current buffer.
If COMPILEP is non-nil then use compilation, otherwise consulting."
  (save-some-buffers)
  (des-ensure-process)
  (let ((filename (des-bsts buffer-file-name)))
    (process-send-string
     "des" (des-build-des-command 
               compilep filename filename))
    (des-goto-des-process-buffer)))


;;------------------------------------------------------------
;; Consulting and compiling
;;------------------------------------------------------------

;;; Interactive interface functions, used by both the standard
;;; and the experimental consultation and compilation functions
(defun des-consult-file ()
  "Consult file of current buffer."
  (interactive)
  (if des-use-standard-consult-compile-method-flag
      (des-old-process-file nil)
    (des-consult-compile-file nil)))

(defun des-consult-buffer ()
  "Consult buffer."
  (interactive)
  (if des-use-standard-consult-compile-method-flag
      (des-old-process-buffer nil)
    (des-consult-compile-buffer nil)))

(defun des-consult-region (beg end)
  "Consult region between BEG and END."
  (interactive "r")
  (if des-use-standard-consult-compile-method-flag
      (des-old-process-region nil beg end)
    (des-consult-compile-region nil beg end)))

(defun des-consult-predicate ()
  "Consult the predicate around current point."
  (interactive)
  (if des-use-standard-consult-compile-method-flag
      (des-old-process-predicate nil)
    (des-consult-compile-predicate nil)))

(defun des-compile-file ()
  "Compile file of current buffer."
  (interactive)
  (if des-use-standard-consult-compile-method-flag
      (des-old-process-file t)
    (des-consult-compile-file t)))

(defun des-compile-buffer ()
  "Compile buffer."
  (interactive)
  (if des-use-standard-consult-compile-method-flag
      (des-old-process-buffer t)
    (des-consult-compile-buffer t)))

(defun des-compile-region (beg end)
  "Compile region between BEG and END."
  (interactive "r")
  (if des-use-standard-consult-compile-method-flag
      (des-old-process-region t beg end)
    (des-consult-compile-region t beg end)))

(defun des-compile-predicate ()
  "Compile the predicate around current point."
  (interactive)
  (if des-use-standard-consult-compile-method-flag
      (des-old-process-predicate t)
    (des-consult-compile-predicate t)))

(defun des-buffer-module ()
  "Select Des module name appropriate for current buffer.
Bases decision on buffer contents (-*- line)."
  ;; Look for -*- ... module: MODULENAME; ... -*-
  (let (beg end)
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t")
      (and (search-forward "-*-" (save-excursion (end-of-line) (point)) t)
           (progn
             (skip-chars-forward " \t")
             (setq beg (point))
             (search-forward "-*-" (save-excursion (end-of-line) (point)) t))
           (progn
             (forward-char -3)
             (skip-chars-backward " \t")
             (setq end (point))
             (goto-char beg)
             (and (let ((case-fold-search t))
                    (search-forward "module:" end t))
                  (progn
                    (skip-chars-forward " \t")
                    (setq beg (point))
                    (if (search-forward ";" end t)
                        (forward-char -1)
                      (goto-char end))
                    (skip-chars-backward " \t")
                    (buffer-substring beg (point)))))))))

(defun des-build-des-command (compilep file buffername 
                                    &optional first-line)
  "Make Des command for FILE compilation/consulting.
If COMPILEP is non-nil, consider compilation, otherwise consulting."
  (let* ((compile-string
          (if compilep des-compile-string-i des-consult-string-i))
         (module (des-buffer-module))
         (file-name (concat "'" file "'"))
         (module-name (if module (concat "'" module "'")))
         (module-file (if module
                          (concat module-name ":" file-name)
                        file-name))
         strbeg strend
         (lineoffset (if first-line
                         (- first-line 1)
                       0)))

    ;; Assure that there is a buffer name
    (if (not buffername)
        (error "The buffer is not saved"))

    (if (not (string-match "^'.*'$" buffername)) ; Add quotes
        (setq buffername (concat "'" buffername "'")))
    (while (string-match "%m" compile-string)
      (setq strbeg (substring compile-string 0 (match-beginning 0)))
      (setq strend (substring compile-string (match-end 0)))
      (setq compile-string (concat strbeg module-file strend)))
    (while (string-match "%f" compile-string)
      (setq strbeg (substring compile-string 0 (match-beginning 0)))
      (setq strend (substring compile-string (match-end 0)))
      (setq compile-string (concat strbeg file-name strend)))
    (while (string-match "%b" compile-string)
      (setq strbeg (substring compile-string 0 (match-beginning 0)))
      (setq strend (substring compile-string (match-end 0)))
      (setq compile-string (concat strbeg buffername strend)))
    (while (string-match "%l" compile-string)
      (setq strbeg (substring compile-string 0 (match-beginning 0)))
      (setq strend (substring compile-string (match-end 0)))
      (setq compile-string (concat strbeg (format "%d" lineoffset) strend)))
    (concat compile-string "\n")))

;;; The rest of this page is experimental code!

;; Global variables for process filter function
(defvar des-process-flag nil
  "Non-nil means that a des task (i.e. a consultation or compilation job) 
is running.")
(defvar des-consult-compile-output ""
  "Hold the unprocessed output from the current des task.")
(defvar des-consult-compile-first-line 1
  "The number of the first line of the file to consult/compile.
Used for temporary files.")
(defvar des-consult-compile-file nil
  "The file to compile/consult (can be a temporary file).")
(defvar des-consult-compile-real-file nil
  "The file name of the buffer to compile/consult.")

(defun des-consult-compile (compilep file &optional first-line)
  "Consult/compile FILE.
If COMPILEP is non-nil, perform compilation, otherwise perform CONSULTING.
COMMAND is a string described by the variables `des-consult-string'
and `des-compile-string'.
Optional argument FIRST-LINE is the number of the first line in the compiled
region.

This function must be called from the source code buffer."
  (if des-process-flag
      (error "Another Des task is running."))
  (des-ensure-process t)
  (let* ((buffer (get-buffer-create des-compilation-buffer))
         (real-file buffer-file-name)
         (command-string (des-build-des-command compilep file 
                                                      real-file first-line))
         (process (get-process "des"))
         (old-filter (process-filter process)))
    (save-excursion
      (set-buffer buffer)
      (delete-region (point-min) (point-max))
      (compilation-mode)
      ;; Setting up font-locking for this buffer
      (make-local-variable 'font-lock-defaults)
      (setq font-lock-defaults 
            '(des-font-lock-keywords nil nil ((?_ . "w"))))
      (if (eq des-system 'sicstus)
          (progn
            (make-local-variable 'compilation-parse-errors-function)
            (setq compilation-parse-errors-function
                  'des-parse-sicstus-compilation-errors)))
      (toggle-read-only 0)
      (insert command-string "\n"))
    (save-selected-window
      (pop-to-buffer buffer))
    (setq des-process-flag t
          des-consult-compile-output ""
          des-consult-compile-first-line (if first-line (1- first-line) 0)
          des-consult-compile-file file
          des-consult-compile-real-file (if (string= 
                                                file buffer-file-name)
                                               nil
                                             real-file))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-max))
      (set-process-filter process 'des-consult-compile-filter)
      (process-send-string "des" command-string)
      ;; (des-build-des-command compilep file real-file first-line))
      (while (and des-process-flag
                  (accept-process-output process 10)) ; 10 secs is ok?
        (sit-for 0.1)
        (unless (get-process "des")
            (setq des-process-flag nil)))
      (insert (if compilep
                  "\nCompilation finished.\n"
                "\nConsulted.\n"))
      (set-process-filter process old-filter))))

(defun des-parse-sicstus-compilation-errors (limit)
  "Parse the des compilation buffer for errors.
Argument LIMIT is a buffer position limiting searching.
For use with the `compilation-parse-errors-function' variable."
  (setq compilation-error-list nil)
  (message "Parsing SICStus error messages...")
  (let (filepath dir file errorline)
    (while 
        (re-search-backward
         "{\\([a-zA-Z ]* ERROR\\|Warning\\):.* in line[s ]*\\([0-9]+\\)"
         limit t)
      (setq errorline (string-to-number (match-string 2)))
      (save-excursion
        (re-search-backward
         "{\\(consulting\\|compiling\\|processing\\) \\(.*\\)\\.\\.\\.}"
         limit t)
        (setq filepath (match-string 2)))

      ;; ###### Does this work with SICStus under Windows (i.e. backslahes and stuff?)
      (if (string-match "\\(.*/\\)\\([^/]*\\)$" filepath)
          (progn
            (setq dir (match-string 1 filepath))
            (setq file (match-string 2 filepath))))

      (setq compilation-error-list
            (cons
             (cons (save-excursion
                     (beginning-of-line)
                     (point-marker))
                   (list (list file dir) errorline))
             compilation-error-list)
            ))
    ))

(defun des-consult-compile-filter (process output)
  "Filter function for Des compilation PROCESS.
Argument OUTPUT is a name of the output file."
  ;;(message "start")
  (setq des-consult-compile-output
        (concat des-consult-compile-output output))
  ;;(message "pccf1: %s" des-consult-compile-output)
  ;; Iterate through the lines of des-consult-compile-output
  (let (outputtype)
    (while (and des-process-flag
                (or
                 ;; Trace question
                 (progn 
                   (setq outputtype 'trace)
                   (and (eq des-system 'sicstus)
                        (string-match
                         "^[ \t]*[0-9]+[ \t]*[0-9]+[ \t]*Call:.*? "
                         des-consult-compile-output)))
                 
                 ;; Match anything
                 (progn 
                   (setq outputtype 'normal)
                   (string-match "^.*\n" des-consult-compile-output))
                   ))
      ;;(message "outputtype: %s" outputtype)

      (setq output (match-string 0 des-consult-compile-output))
      ;; remove the text in output from des-consult-compile-output
      (setq des-consult-compile-output
            (substring des-consult-compile-output (length output)))
      ;;(message "pccf2: %s" des-consult-compile-output)
      
      ;; If temporary files were used, then we change the error
      ;; messages to point to the original source file.
      (cond

       ;; If the des process was in trace mode then it requires
       ;; user input
       ((and (eq des-system 'sicstus) 
             (eq outputtype 'trace))
        (let (input)
          (setq input (concat (read-string output) "\n"))
          (process-send-string "des" input)
          (setq output (concat output input))))

       ((eq des-system 'sicstus)
        (if (and des-consult-compile-real-file
                 (string-match
                  "\\({.*:.* in line[s ]*\\)\\([0-9]+\\)-\\([0-9]+\\)" output))
            (setq output (replace-match
                          ;; Adds a {processing ...} line so that 
                          ;; `des-parse-sicstus-compilation-errors'
                          ;; finds the real file instead of the temporary one.
                          ;; Also fixes the line numbers.
                          (format "Added by Emacs: {processing %s...}\n%s%d-%d"
                                  des-consult-compile-real-file
                                  (match-string 1 output)
                                  (+ des-consult-compile-first-line
                                     (string-to-number
                                      (match-string 2 output)))
                                  (+ des-consult-compile-first-line
                                     (string-to-number
                                      (match-string 3 output))))
                          t t output)))
        )
       
       ((eq des-system 'swi)
        (if (and des-consult-compile-real-file
                 (string-match (format
                                "%s\\([ \t]*:[ \t]*\\)\\([0-9]+\\)"
                                des-consult-compile-file)
                               output))
            (setq output (replace-match
                          ;; Real filename + text + fixed linenum
                          (format "%s%s%d"
                                  des-consult-compile-real-file
                                  (match-string 1 output)
                                  (+ des-consult-compile-first-line
                                     (string-to-number
                                      (match-string 2 output))))
                          t t output)))
        )
       
       (t ())
       )
      ;; Write the output in the *des-compilation* buffer
      (insert output)))

  ;; If the prompt is visible, then the task is finished
  (if (string-match des-prompt-regexp-i des-consult-compile-output)
      (setq des-process-flag nil)))

(defun des-consult-compile-file (compilep)
  "Consult/compile file of current buffer.
If COMPILEP is non-nil, compile, otherwise consult."
  (let ((file buffer-file-name))
    (if file
        (progn
          (save-some-buffers)
          (des-consult-compile compilep file))
      (des-consult-compile-region compilep (point-min) (point-max)))))

(defun des-consult-compile-buffer (compilep)
  "Consult/compile current buffer.
If COMPILEP is non-nil, compile, otherwise consult."
  (des-consult-compile-region compilep (point-min) (point-max)))

(defun des-consult-compile-region (compilep beg end)
  "Consult/compile region between BEG and END.
If COMPILEP is non-nil, compile, otherwise consult."
  ;(let ((file des-temp-filename)
  (let ((file (des-bsts (des-temporary-file)))
        (lines (count-lines 1 beg)))
    (write-region beg end file nil 'no-message)
    (write-region "\n" nil file t 'no-message)
    (des-consult-compile compilep file
                            (if (looking-at "^") (1+ lines) lines))
    (delete-file file)))

(defun des-consult-compile-predicate (compilep)
  "Consult/compile the predicate around current point.
If COMPILEP is non-nil, compile, otherwise consult."
  (des-consult-compile-region
   compilep (des-pred-start) (des-pred-end)))


;;-------------------------------------------------------------------
;; Font-lock stuff
;;-------------------------------------------------------------------

;; Auxilliary functions
(defun des-make-keywords-regexp (keywords &optional protect)
  "Create regexp from the list of strings KEYWORDS.
If PROTECT is non-nil, surround the result regexp by word breaks."
  (let ((regexp
         (if (fboundp 'regexp-opt)
             ;; Emacs 20
             ;; Avoid compile warnings under earlier versions by using eval
             (eval '(regexp-opt keywords))
           ;; Older Emacsen
           (concat (mapconcat 'regexp-quote keywords "\\|")))
         ))
    (if protect
        (concat "\\<\\(" regexp "\\)\\>")
      regexp)))

(defun des-font-lock-object-matcher (bound)
  "Find SICStus objects method name for font lock.
Argument BOUND is a buffer position limiting searching."
  (let (point
         (case-fold-search nil))
    (while (and (not point)
                (re-search-forward "\\(::[ \t\n]*{\\|&\\)[ \t]*"
                                   bound t))
      (while (or (re-search-forward "\\=\n[ \t]*" bound t)
                  (re-search-forward "\\=%.*" bound t)
                  (and (re-search-forward "\\=/\\*" bound t)
                       (re-search-forward "\\*/[ \t]*" bound t))))
      (setq point (re-search-forward
                   (format "\\=\\(%s\\)" des-atom-regexp)
                   bound t)))
    point))

;; Set everything up
(defun des-font-lock-keywords ()
  "Set up font lock keywords for the current Des system."
  ;(when window-system
    (require 'font-lock)
    
    ;; Define Des faces
    (defface des-redo-face
      '((((class grayscale)) (:italic t))
        (((class color)) (:foreground "darkorchid"))
        (t (:italic t)))
      "Des mode face for highlighting redo trace lines."
      :group 'des-faces)
    (defface des-exit-face
      '((((class grayscale)) (:underline t))
        (((class color) (background dark)) (:foreground "green"))
        (((class color) (background light)) (:foreground "ForestGreen"))
        (t (:underline t)))
      "Des mode face for highlighting exit trace lines."
      :group 'des-faces)
    (defface des-exception-face
      '((((class grayscale)) (:bold t :italic t :underline t))
        (((class color)) (:bold t :foreground "black" :background "Khaki"))
        (t (:bold t :italic t :underline t)))
      "Des mode face for highlighting exception trace lines."
      :group 'des-faces)
    (defvar des-warning-face 'font-lock-warning-face
      "Face name to use for compiler warnings.")
    (defvar des-builtin-face 'font-lock-builtin-face
      "Face name to use for built in predicates.")
    (defvar des-redo-face 'des-redo-face
      "Face name to use for redo trace lines.")
    (defvar des-exit-face 'des-exit-face
      "Face name to use for exit trace lines.")
    (defvar des-exception-face 'des-exception-face
      "Face name to use for exception trace lines.")
    
    ;; Font Lock Patterns
    (let (
          ;; "Native" Des patterns
          (head-predicates
           (list (format "^%s" des-atom-regexp)
                 0 font-lock-function-name-face))
          (variables
           '("\\<\\([_A-Z][a-zA-Z0-9_]*\\)"
             1 font-lock-variable-name-face))
          (important-elements
           (list (if (eq des-system 'mercury)
                     "[][}{;|]\\|\\\\[+=]\\|<?=>?"
                   "[][}{!;|]\\|\\*->")
                 0 'font-lock-keyword-face))
          (important-elements-1
           '("[^-*]\\(->\\)" 1 font-lock-keyword-face))
          (predspecs                        ; module:predicate/cardinality
           (list (format "\\<\\(%s:\\|\\)%s/[0-9]+"
                         des-atom-regexp des-atom-regexp)
                 0 font-lock-function-name-face 'prepend))
          (keywords                        ; directives (queries)
           (list
            (if (eq des-system 'mercury)
                (concat
                 "\\<\\("
                 (des-make-keywords-regexp des-keywords-i)
                 "\\|"
                 (des-make-keywords-regexp
                  des-determinism-specificators-i)
                 "\\)\\>")
              (concat
               "^[?:]- *\\("
               (des-make-keywords-regexp des-keywords-i)
               "\\)\\>"))
              1 des-builtin-face))
          (quoted_atom (list des-quoted-atom-regexp
                             2 'font-lock-string-face 'append))
          (string (list des-string-regexp
                        1 'font-lock-string-face 'append))
          ;; SICStus specific patterns
          (sicstus-object-methods
           (if (eq des-system 'sicstus)
               '(des-font-lock-object-matcher
                 1 font-lock-function-name-face)))
          ;; Mercury specific patterns
          (types
           (if (eq des-system 'mercury)
               (list
                (des-make-keywords-regexp des-types-i t)
                0 'font-lock-type-face)))
          (modes
           (if (eq des-system 'mercury)
               (list
                (des-make-keywords-regexp des-mode-specificators-i t)
                0 'font-lock-reference-face)))
          (directives
           (if (eq des-system 'mercury)
               (list
                (des-make-keywords-regexp des-directives-i t)
                0 'des-warning-face)))
          ;; Inferior mode specific patterns
          (prompt
           (list des-prompt-regexp-i 0 'font-lock-keyword-face))
          (trace-exit
           (cond
            ((eq des-system 'sicstus)
             '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Exit\\):"
               1 des-exit-face))
            ((eq des-system 'swi)
             '("[ \t]*\\(Exit\\):[ \t]*([ \t0-9]*)" 1 des-exit-face))
            (t nil)))
          (trace-fail
           (cond
            ((eq des-system 'sicstus)
             '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Fail\\):"
               1 des-warning-face))
            ((eq des-system 'swi)
             '("[ \t]*\\(Fail\\):[ \t]*([ \t0-9]*)" 1 des-warning-face))
            (t nil)))
          (trace-redo
           (cond
            ((eq des-system 'sicstus)
             '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Redo\\):"
               1 des-redo-face))
            ((eq des-system 'swi)
             '("[ \t]*\\(Redo\\):[ \t]*([ \t0-9]*)" 1 des-redo-face))
            (t nil)))
          (trace-call
           (cond
            ((eq des-system 'sicstus)
             '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Call\\):"
               1 font-lock-function-name-face))
            ((eq des-system 'swi)
             '("[ \t]*\\(Call\\):[ \t]*([ \t0-9]*)"
               1 font-lock-function-name-face))
            (t nil)))
          (trace-exception
           (cond
            ((eq des-system 'sicstus)
             '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Exception\\):"
               1 des-exception-face))
            ((eq des-system 'swi)
             '("[ \t]*\\(Exception\\):[ \t]*([ \t0-9]*)"
               1 des-exception-face))
            (t nil)))
          (error-message-identifier
           (cond
            ((eq des-system 'sicstus)
             '("{\\([A-Z]* ?ERROR:\\)" 1 des-exception-face prepend))
            ((eq des-system 'swi)
             '("^[[]\\(WARNING:\\)" 1 des-builtin-face prepend))
            (t nil)))
          (error-whole-messages
           (cond
            ((eq des-system 'sicstus)
             '("{\\([A-Z]* ?ERROR:.*\\)}[ \t]*$"
               1 font-lock-comment-face append))
            ((eq des-system 'swi)
             '("^[[]WARNING:[^]]*[]]$" 0 font-lock-comment-face append))
            (t nil)))
          (error-warning-messages
           ;; Mostly errors that SICStus asks the user about how to solve,
           ;; such as "NAME CLASH:" for example.
           (cond
            ((eq des-system 'sicstus)
             '("^[A-Z ]*[A-Z]+:" 0 des-warning-face))
            (t nil)))
          (warning-messages
           (cond
            ((eq des-system 'sicstus)
             '("\\({ ?\\(Warning\\|WARNING\\) ?:.*}\\)[ \t]*$" 
               2 des-warning-face prepend))
            (t nil))))

      ;; Make font lock list
      (delq
       nil
       (cond
        ((eq major-mode 'des-mode)
         (list
          head-predicates
          quoted_atom
          string
          variables
          important-elements
          important-elements-1
          predspecs
          keywords
          sicstus-object-methods
          types
          modes
          directives))
        ((eq major-mode 'des-inferior-mode)
         (list
         prompt
         error-message-identifier
         error-whole-messages
         error-warning-messages
         warning-messages
         predspecs
         trace-exit
         trace-fail
         trace-redo
         trace-call
         trace-exception))
        ((eq major-mode 'compilation-mode)
         (list
         error-message-identifier
         error-whole-messages
         error-warning-messages
         warning-messages
         predspecs))))
      ))


;;-------------------------------------------------------------------
;; Indentation stuff
;;-------------------------------------------------------------------

;; NB: This function *MUST* have this optional argument since XEmacs
;; assumes it. This does not mean we have to use it...
(defun des-indent-line (&optional whole-exp)
  "Indent current line as Des code.
With argument, indent any additional lines of the same clause
rigidly along with this one (not yet)."
  (interactive "p")
  (let ((indent (des-indent-level))
        (pos (- (point-max) (point))) beg)
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (zerop (- indent (current-column)))
        nil
      (delete-region beg (point))
      (indent-to indent))
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))
    
    ;; Align comments
    (if des-align-comments-flag
        (save-excursion
          (des-goto-comment-column t)))

    ;; Insert spaces if needed
    (if (or des-electric-tab-flag des-electric-if-then-else-flag)
        (des-insert-spaces-after-paren))
    ))

(defun des-comment-indent ()
  "Compute des comment indentation."
  (cond ((looking-at "%%%") (des-indentation-level-of-line))
        ((looking-at "%%") (des-indent-level))
        (t
         (save-excursion
           (skip-chars-backward " \t")
           ;; Insert one space at least, except at left margin.
           (max (+ (current-column) (if (bolp) 0 1))
                comment-column)))
        ))

(defun des-indent-level ()
  "Compute des indentation level."
  (save-excursion
    (beginning-of-line)
    (let ((totbal (des-region-paren-balance
                   (des-clause-start t) (point)))
          (oldpoint (point)))
      (skip-chars-forward " \t")
      (cond
       ((looking-at "%%%") (des-indentation-level-of-line))
                                        ;Large comment starts
       ((looking-at "%[^%]") comment-column) ;Small comment starts
       ((bobp) 0)                        ;Beginning of buffer

       ;; If we found '}' then we must check if it's the
       ;; end of an object declaration or something else.
       ((and (looking-at "}") 
             (save-excursion
               (forward-char 1)
               ;; Goto to matching {
               (if des-use-des-tokenizer-flag
                   (des-backward-list)
                 (backward-list))
               (skip-chars-backward " \t")
               (backward-char 2)
               (looking-at "::")))
        ;; It was an object
        (if des-object-end-to-0-flag
            0
          des-indent-width)) 

       ;;End of /* */ comment
       ((looking-at "\\*/")                  
        (save-excursion
          (des-find-start-of-mline-comment)
          (skip-chars-backward " \t")
          (- (current-column) 2)))

       ;; Here we check if the current line is within a /* */ pair
       ((and (looking-at "[^%/]")
             (eq (des-in-string-or-comment) 'cmt)) 
        (if des-indent-mline-comments-flag
            (des-find-start-of-mline-comment)
          ;; Same as before
          (des-indentation-level-of-line)))

       (t
        (let ((empty t) ind linebal)
          ;; See previous indentation
          (while empty
            (forward-line -1)
            (beginning-of-line)
            (if (= (point) (point-min))
                (setq empty nil)
              (skip-chars-forward " \t")
              (if (not (or (not (member (des-in-string-or-comment) '(nil txt)))
                           (looking-at "%") 
                           (looking-at "\n")))
                  (setq empty nil))))

          ;; Store this line's indentation
          (if (= (point) (point-min))
              (setq ind 0)                ;Beginning of buffer
            (setq ind (current-column))) ;Beginning of clause

          ;; Compute the balance of the line
          (setq linebal (des-paren-balance))
          ;;(message "bal of previous line %d totbal %d" linebal totbal)
          (if (< linebal 0)
              (progn
                ;; Add 'indent-level' mode to find-unmatched-paren instead?
                (end-of-line)
                (setq ind (des-find-indent-of-matching-paren))))

          ;;(message "ind %d" ind)
          (beginning-of-line)

          ;; Check if the line ends with ":-", ".", ":: {", "}" (might be
          ;; unnecessary), "&" or ")" (The last four concerns SICStus objects)
          (cond
           ;; If the last char of the line is a '&' then set the indent level
           ;; to des-indent-width (used in SICStus objects)
           ((and (eq des-system 'sicstus) 
                 (looking-at ".+&[ \t]*\\(%.*\\|\\)$"))
            (setq ind des-indent-width))

           ;; Increase indentation if the previous line was the head of a rule
           ;; and does not contain a '.'
           ((and (looking-at (format ".*%s[^\\.]*[ \t]*\\(%%.*\\|\\)$" 
                                     des-head-delimiter))
                 ;; We must check that the match is at a paren balance of 0.
                 (save-excursion
                   (let ((p (point)))
                     (re-search-forward des-head-delimiter)
                     (>= 0 (des-region-paren-balance p (point))))))
            (let (headindent)
              (if (< (des-paren-balance) 0)
                  (save-excursion
                    (end-of-line)
                    (setq headindent (des-find-indent-of-matching-paren)))
                (setq headindent (des-indentation-level-of-line)))
              (setq ind (+ headindent des-indent-width))))

           ;; The previous line was the head of an object
           ((looking-at ".+ *::.*{[ \t]*$")
            (setq ind des-indent-width))

           ;; If a '.' is found at the end of the previous line, then
           ;; decrease the indentation. (The \\(%.*\\|\\) part of the
           ;; regexp is for comments at the end of the line)
           ((and (looking-at "^.+\\.[ \t]*\\(%.*\\|\\)$") 
                 ;; Make sure that the '.' found is not in a comment or string
                 (save-excursion
                   (end-of-line)
                   (re-search-backward "\\.[ \t]*\\(%.*\\|\\)$" (point-min))
                   ;; Guard against the real '.' being followed by a
                   ;; commented '.'.
                   (if (eq (des-in-string-or-comment) 'cmt)  ;; commented out '.'
                       (let ((here (save-excursion
                                     (beginning-of-line)
                                     (point))))
                         (end-of-line)
                         (re-search-backward "\\.[ \t]*%.*$" here t))
                     (not (des-in-string-or-comment))
                     )
                   ))
            (setq ind 0))

           ;; If a '.' is found at the end of the previous line, then
           ;; decrease the indentation. (The /\\*.*\\*/ part of the
           ;; regexp is for C-like comments at the end of the
           ;; line--can we merge with the case above?).
           ((and (looking-at "^.+\\.[ \t]*\\(/\\*.*\\|\\)$") 
                 ;; Make sure that the '.' found is not in a comment or string
                 (save-excursion
                   (end-of-line)
                   (re-search-backward "\\.[ \t]*\\(/\\*.*\\|\\)$" (point-min))
                   ;; Guard against the real '.' being followed by a
                   ;; commented '.'.
                   (if (eq (des-in-string-or-comment) 'cmt)  ;; commented out '.'
                       (let ((here (save-excursion
                                     (beginning-of-line)
                                     (point))))
                         (end-of-line)
                         (re-search-backward "\\.[ \t]*/\\*.*$" here t))
                     (not (des-in-string-or-comment))
                     )
                   ))
            (setq ind 0))

           )

          ;; If the last non comment char is a ',' or left paren or a left-
          ;; indent-regexp then indent to open parenthesis level
          (if (and
               (> totbal 0)
               ;; SICStus objects have special syntax rules if point is
               ;; not inside additional parens (objects are defined
               ;; within {...})
               (not (and (eq des-system 'sicstus)
                         (= totbal 1)
                         (des-in-object))))
              (if (looking-at
                   (format "\\(%s\\|%s\\|0'.\\|[0-9]+'[0-9a-zA-Z]+\\|[^\n\'\"%%]\\)*\\(,\\|%s\\|%s\\)\[ \t]*\\(%%.*\\|\\)$" 
                           des-quoted-atom-regexp des-string-regexp
                           des-left-paren des-left-indent-regexp))
                  (progn
                    (goto-char oldpoint)
                    (setq ind (des-find-unmatched-paren (if des-paren-indent-p 
                                                               'termdependent 
                                                             'skipwhite)))
                    ;;(setq ind (des-find-unmatched-paren 'termdependent))
                    )
                (goto-char oldpoint)
                (setq ind (des-find-unmatched-paren nil))
                ))
          

          ;; Return the indentation level
          ind
          ))))))

(defun des-find-indent-of-matching-paren ()
  "Find the indentation level based on the matching parenthesis.
Indentation level is set to the one the point is after when the function is
called."
  (save-excursion
    ;; Go to the matching paren
    (if des-use-des-tokenizer-flag
        (des-backward-list)
      (backward-list))

    ;; If this was the first paren on the line then return this line's
    ;; indentation level
    (if (des-paren-is-the-first-on-line-p)
        (des-indentation-level-of-line)
      ;; It was not the first one
      (progn
         ;; Find the next paren
         (des-goto-next-paren 0)

         ;; If this paren is a left one then use its column as indent level,
         ;; if not then recurse this function
         (if (looking-at des-left-paren)
             (+ (current-column) 1)
           (progn
              (forward-char 1)
              (des-find-indent-of-matching-paren)))
         ))
    ))

(defun des-indentation-level-of-line ()
  "Return the indentation level of the current line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-column)))

(defun des-first-pos-on-line ()
  "Return the first position on the current line."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun des-paren-is-the-first-on-line-p ()
  "Return t if the parenthesis under the point is the first one on the line.
Return nil otherwise.
Note: does not check if the point is actually at a parenthesis!"
  (save-excursion
    (let ((begofline (des-first-pos-on-line)))
      (if (= begofline (point))
          t
        (if (des-goto-next-paren begofline)
            nil
          t)))))

(defun des-find-unmatched-paren (&optional mode)
  "Return the column of the last unmatched left parenthesis.
If MODE is `skipwhite' then any white space after the parenthesis is added to
the answer.
If MODE is `plusone' then the parenthesis' column +1 is returned.
If MODE is `termdependent' then if the unmatched parenthesis is part of
a compound term the function will work as `skipwhite', otherwise
it will return the column paren plus the value of `des-paren-indent'.
If MODE is nil or not set then the parenthesis' exact column is returned."
  (save-excursion
    ;; If the next paren we find is a left one we're finished, if it's
    ;; a right one then we go back one step and recurse
    (des-goto-next-paren 0)

    (let ((roundparen (looking-at "(")))
      (if (looking-at des-left-paren)
          (let ((not-part-of-term 
                 (save-excursion
                   (backward-char 1)
                   (looking-at "[ \t]"))))
            (if (eq mode nil)
                (current-column)
              (if (and roundparen
                       (eq mode 'termdependent) 
                       not-part-of-term)
                  (+ (current-column)
                     (if des-electric-tab-flag
                         ;; Electric TAB
                         des-paren-indent
                       ;; Not electric TAB
                       (if (looking-at ".[ \t]*$")
                           2
                         des-paren-indent))
                     )

                (forward-char 1)
                (if (or (eq mode 'skipwhite) (eq mode 'termdependent) )
                    (skip-chars-forward " \t"))
                (current-column))))
        ;; Not looking at left paren
        (progn
          (forward-char 1)
          ;; Go to the matching paren. When we get there we have a total
          ;; balance of 0.
          (if des-use-des-tokenizer-flag
              (des-backward-list)
            (backward-list))
          (des-find-unmatched-paren mode)))
      )))


(defun des-paren-balance ()
  "Return the parenthesis balance of the current line.
A return value of n means n more left parentheses than right ones."
  (save-excursion
    (end-of-line)
    (des-region-paren-balance (des-first-pos-on-line) (point))))

(defun des-region-paren-balance (beg end)
  "Return the summed parenthesis balance in the region.
The region is limited by BEG and END positions."
  (save-excursion
    (let ((state (if des-use-des-tokenizer-flag
                     (des-tokenize beg end)
                   (parse-partial-sexp beg end))))
      (nth 0 state))))

(defun des-goto-next-paren (limit-pos)
  "Move the point to the next parenthesis earlier in the buffer.
Return t if a match was found before LIMIT-POS.  Return nil otherwise."
  (let (retval)
    (setq retval (re-search-backward
                  (concat des-left-paren "\\|" des-right-paren)
                  limit-pos t))

    ;; If a match was found but it was in a string or comment, then recurse
    (if (and retval (des-in-string-or-comment))
        (des-goto-next-paren limit-pos)
      retval)
    ))

(defun des-in-string-or-comment ()
  "Check whether string, atom, or comment is under current point.
Return:
 `txt' if the point is in a string, atom, or character code expression
 `cmt' if the point is in a comment
 nil otherwise."
  (save-excursion
    (let* ((start
            (if (eq des-parse-mode 'beg-of-line)
                ;; 'beg-of-line
                (save-excursion
                  (let (safepoint)
                    (beginning-of-line)
                    (setq safepoint (point))
                    (while (and (> (point) (point-min))
                                (progn
                                  (forward-line -1)
                                  (end-of-line)
                                  (if (not (bobp))
                                      (backward-char 1))
                                  (looking-at "\\\\"))
                                )
                      (beginning-of-line)
                      (setq safepoint (point)))
                    safepoint))
              ;; 'beg-of-clause
              (des-clause-start)))
           (end (point))
           (state (if des-use-des-tokenizer-flag
                      (des-tokenize start end)
                    (parse-partial-sexp start end))))
      (cond
       ((nth 3 state) 'txt) ; String
       ((nth 4 state) 'cmt) ; Comment
       (t
        (cond
         ((looking-at "%") 'cmt) ; Start of a comment
         ((looking-at "/\\*") 'cmt) ; Start of a comment
         ((looking-at "\'") 'txt) ; Start of an atom
         ((looking-at "\"") 'txt) ; Start of a string
         (t nil)
         ))))
    ))

(defun des-find-start-of-mline-comment ()
  "Return the start column of a /* */ comment.
This assumes that the point is inside a comment."
  (re-search-backward "/\\*" (point-min) t)
  (forward-char 2)
  (skip-chars-forward " \t")
  (current-column))

(defun des-insert-spaces-after-paren ()
  "Insert spaces after the opening parenthesis, \"then\" (->) and \"else\" (;) branches.
Spaces are inserted if all preceding objects on the line are
whitespace characters, parentheses, or then/else branches."
  (save-excursion
    (let ((regexp (concat "(\\|" des-left-indent-regexp))
          level)
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if (looking-at regexp)
          (progn
            ;; Treat "( If -> " lines specially.
            ;(if (looking-at "(.*->")
            ;    (setq incr 2)
            ;  (setq incr des-paren-indent))

	    ;; work on all subsequent "->", "(", ";"
	    (while (looking-at regexp)
	      (goto-char (match-end 0))
	      (setq level (+ (des-find-unmatched-paren) des-paren-indent))

	      ;; Remove old white space
	      (let ((start (point)))
		(skip-chars-forward " \t")
		(delete-region start (point)))
	      (indent-to level)
	      (skip-chars-forward " \t"))
	    ))))
  (skip-chars-forward " \t"))

;;;; Comment filling

(defun des-comment-limits ()
  "Returns the current comment limits plus the comment type (block or line).
The comment limits are the range of a block comment or the range that
contains all adjacent line comments (i.e. all comments that starts in
the same column with no empty lines or non-whitespace characters
between them)."
(let ((here (point))
      lit-limits-b lit-limits-e lit-type beg end
      )
  (save-restriction
    ;; Widen to catch comment limits correctly.
    (widen)
    (setq end (save-excursion (end-of-line) (point))
          beg (save-excursion (beginning-of-line) (point)))
    (save-excursion
      (beginning-of-line)
      (setq lit-type (if (search-forward-regexp "%" end t) 'line 'block))
      ;    (setq lit-type 'line)
      ;(if (search-forward-regexp "^[ \t]*%" end t)
      ;    (setq lit-type 'line)
      ;  (if (not (search-forward-regexp "%" end t))
      ;      (setq lit-type 'block)
      ;    (if (not (= (forward-line 1) 0))
      ;        (setq lit-type 'block)
      ;      (setq done t
      ;            ret (des-comment-limits)))
      ;    ))
      (if (eq lit-type 'block)
          (progn
            (goto-char here)
            (when (looking-at "/\\*") (forward-char 2))
            (when (and (looking-at "\\*") (> (point) (point-min)) 
                       (forward-char -1) (looking-at "/"))
              (forward-char 1))
            (when (save-excursion (search-backward "/*" nil t))
              (list (save-excursion (search-backward "/*") (point))
                    (or (search-forward "*/" nil t) (point-max)) lit-type)))
        ;; line comment
        (setq lit-limits-b (- (point) 1) 
              lit-limits-e end)
        (condition-case nil
            (if (progn (goto-char lit-limits-b)
                       (looking-at "%"))
                (let ((col (current-column)) done)
                  (setq beg (point)
                        end lit-limits-e)
                  ;; Always at the beginning of the comment
                  ;; Go backward now
                  (beginning-of-line)
                  (while (and (zerop (setq done (forward-line -1)))
                              (search-forward-regexp "^[ \t]*%" (save-excursion (end-of-line) (point)) t)
                              (= (+ 1 col) (current-column)))
                    (setq beg (- (point) 1)))
                  (when (= done 0)
                    (forward-line 1))
                  ;; We may have a line with code above...
                  (when (and (zerop (setq done (forward-line -1)))
                             (search-forward "%" (save-excursion (end-of-line) (point)) t)
                             (= (+ 1 col) (current-column)))
                    (setq beg (- (point) 1)))
                  (when (= done 0)
                    (forward-line 1))
                  ;; Go forward
                  (goto-char lit-limits-b)
                  (beginning-of-line)
                  (while (and (zerop (forward-line 1))
                              (search-forward-regexp "^[ \t]*%" (save-excursion (end-of-line) (point)) t)
                              (= (+ 1 col) (current-column)))
                    (setq end (save-excursion (end-of-line) (point))))
                  (list beg end lit-type))
              (list lit-limits-b lit-limits-e lit-type)
              )
          (error (list lit-limits-b lit-limits-e lit-type))))
      ))))

(defun des-guess-fill-prefix ()
  ;; fill 'txt entities?
  (when (save-excursion
          (end-of-line)
          (equal (des-in-string-or-comment) 'cmt))
    (let* ((bounds (des-comment-limits))
           (cbeg (car bounds))
           (type (nth 2 bounds))
           beg end str)
      (save-excursion
        (end-of-line)
        (setq end (point))
        (beginning-of-line)
        (setq beg (point))
        (if (and (eq type 'line)
                 (> cbeg beg)
                 (save-excursion (not (search-forward-regexp "^[ \t]*%" cbeg t))))
            (progn
              (goto-char cbeg)
              (search-forward-regexp "%+[ \t]*" end t)
              (setq str (replace-in-string (buffer-substring beg (point)) "[^ \t%]" " "))
              )
          ;(goto-char beg)
          (if (search-forward-regexp "^[ \t]*\\(%+\\|\\*+\\|/\\*+\\)[ \t]*" end t)
              (setq str (replace-in-string (buffer-substring beg (point)) "/" " "))
            (beginning-of-line)
            (when (search-forward-regexp "^[ \t]+" end t)
              (setq str (buffer-substring beg (point)))))
          ))
      str)))

(defun des-fill-paragraph ()
  "Fill paragraph comment at or after point."
  (interactive)
  (let* ((bounds (des-comment-limits))
         (type (nth 2 bounds)))
    (if (eq type 'line)
        (let ((fill-prefix (des-guess-fill-prefix)))
          (fill-paragraph nil))
      (save-excursion
        (save-restriction
          ;; exclude surrounding lines that delimit a multiline comment
          ;; and don't contain alphabetic characters, like "/*******",
          ;; "- - - */" etc.
          (save-excursion
            (backward-paragraph)
            (unless (bobp) (forward-line))
            (if (string-match "^/\\*[^a-zA-Z]*$" (thing-at-point 'line))
                (narrow-to-region (point-at-eol) (point-max))))
          (save-excursion
            (forward-paragraph)
            (forward-line -1)
            (if (string-match "^[^a-zA-Z]*\\*/$" (thing-at-point 'line))
                (narrow-to-region (point-min) (point-at-bol))))
          (let ((fill-prefix (des-guess-fill-prefix)))
            (fill-paragraph nil))))
      )))

(defun des-do-auto-fill ()
  "Carry out Auto Fill for Des mode.
In effect it sets the fill-prefix when inside comments and then calls
`do-auto-fill'."
  (let ((fill-prefix (des-guess-fill-prefix)))
    (do-auto-fill)
    ))

(unless (fboundp 'replace-in-string)
  (defun replace-in-string (str regexp newtext &optional literal)
    "Replace all matches in STR for REGEXP with NEWTEXT string,
 and returns the new string.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat `\\' in NEWTEXT as special:
  `\\&' in NEWTEXT means substitute original matched text.
  `\\N' means substitute what matched the Nth `\\(...\\)'.
       If Nth parens didn't match, substitute nothing.
  `\\\\' means insert one `\\'.
  `\\u' means upcase the next character.
  `\\l' means downcase the next character.
  `\\U' means begin upcasing all following characters.
  `\\L' means begin downcasing all following characters.
  `\\E' means terminate the effect of any `\\U' or `\\L'."
    (if (> (length str) 50)
        (let ((cfs case-fold-search))
          (with-temp-buffer
            (setq case-fold-search cfs)
            (insert str)
            (goto-char 1)
            (while (re-search-forward regexp nil t)
              (replace-match newtext t literal))
            (buffer-string)))
      (let ((start 0) newstr)
        (while (string-match regexp str start)
          (setq newstr (replace-match newtext t literal str)
                start (+ (match-end 0) (- (length newstr) (length str)))
                str newstr))
        str)))
  )



;;-------------------------------------------------------------------
;; The tokenizer
;;-------------------------------------------------------------------

(defconst des-tokenize-searchkey
  (concat "[0-9]+'"
          "\\|"
          "['\"]"
          "\\|"
          des-left-paren
          "\\|"
          des-right-paren
          "\\|"
          "%"
          "\\|"
          "/\\*"
          ))

(defun des-tokenize (beg end &optional stopcond)
  "Tokenize a region of des code between BEG and END.
STOPCOND decides the stop condition of the parsing. Valid values
are 'zerodepth which stops the parsing at the first right parenthesis
where the parenthesis depth is zero, 'skipover which skips over
the current entity (e.g. a list, a string, etc.) and nil.

The function returns a list with the following information:
 0. parenthesis depth 
 3. 'atm if END is inside an atom
    'str if END is inside a string
    'chr if END is in a character code expression (0'x)
    nil otherwise
 4. non-nil if END is inside a comment
 5. end position (always equal to END if STOPCOND is nil)
The rest of the elements are undefined."
  (save-excursion
    (let* ((end2 (1+ end))
           oldp
           (depth 0)
           (quoted nil)
           inside_cmt
           (endpos end2)
           skiptype ; The type of entity we'll skip over
           )
      (goto-char beg)

      (if (and (eq stopcond 'skipover)
               (looking-at "[^[({'\"]"))
          (setq endpos (point))                ; Stay where we are
        (while (and
                (re-search-forward des-tokenize-searchkey end2 t)
                (< (point) end2))
          (progn
            (setq oldp (point))
            (goto-char (match-beginning 0))
            (cond
             ;; Atoms and strings
             ((looking-at "'")
              ;; Find end of atom
              (if (re-search-forward "[^\\]'" end2 'limit)
                  ;; Found end of atom
                  (progn
                    (setq oldp end2)
                    (if (and (eq stopcond 'skipover)
                             (not skiptype))
                        (setq endpos (point))
                      (setq oldp (point)))) ; Continue tokenizing
                (setq quoted 'atm)))
           
             ((looking-at "\"")
              ;; Find end of string
              (if (re-search-forward "[^\\]\"" end2 'limit)
                  ;; Found end of string
                  (progn
                    (setq oldp end2)
                    (if (and (eq stopcond 'skipover)
                             (not skiptype))
                        (setq endpos (point))
                      (setq oldp (point)))) ; Continue tokenizing
                (setq quoted 'str)))

             ;; Paren stuff
             ((looking-at des-left-paren)
              (setq depth (1+ depth))
              (setq skiptype 'paren))

             ((looking-at des-right-paren)
              (setq depth (1- depth))
              (if (and
                   (or (eq stopcond 'zerodepth)
                       (and (eq stopcond 'skipover) 
                            (eq skiptype 'paren)))
                   (= depth 0))
                  (progn
                    (setq endpos (1+ (point)))
                    (setq oldp end2))))

             ;; Comment stuff
             ((looking-at comment-start)
              (end-of-line)
              ;; (if (>= (point) end2)
              (if (>= (point) end)
                  (progn
                    (setq inside_cmt t)
                    (setq oldp end2))
                (setq oldp (point))))

             ((looking-at "/\\*")
              (if (re-search-forward "\\*/" end2 'limit)
                  (setq oldp (point))
                (setq inside_cmt t)
                (setq oldp end2)))

             ;; 0'char
             ((looking-at "0'")
              (setq oldp (1+ (match-end 0)))
              (if (> oldp end) 
                  (setq quoted 'chr)))
           
             ;; base'number
             ((looking-at "[0-9]+'")
              (goto-char (match-end 0))
              (skip-chars-forward "0-9a-zA-Z")
              (setq oldp (point)))

             
             )
            (goto-char oldp)
            ))                                ; End of while
        )

      ;; Create return list
      (list depth nil nil quoted inside_cmt endpos)
      )))


;;-------------------------------------------------------------------
;; Online help
;;-------------------------------------------------------------------

(defvar des-help-function
  '((mercury nil)
    (eclipse des-help-online)
    ;; (sicstus des-help-info)
    (sicstus des-find-documentation)
    (swi des-help-online)
    (t des-help-online))
  "Alist for the name of the function for finding help on a predicate.")

(defun des-help-on-predicate ()
  "Invoke online help on the atom under cursor."
  (interactive)

  (cond
   ;; Redirect help for SICStus to `des-find-documentation'.
   ((eq des-help-function-i 'des-find-documentation)
    (des-find-documentation))

   ;; Otherwise, ask for the predicate name and then call the function
   ;; in des-help-function-i
   (t
    (let* (word
           predicate
           ;point
           )
      (setq word (des-atom-under-point))
      (setq predicate (read-from-minibuffer
                       (format "Help on predicate%s: "
                               (if word
                                   (concat " (default " word ")")
                                 ""))))
      (if (string= predicate "")
          (setq predicate word))
      (if des-help-function-i
          (funcall des-help-function-i predicate)
        (error "Sorry, no help method defined for this Des system."))))
   ))

(defun des-help-info (predicate)
  (let ((buffer (current-buffer))
        oldp
        (str (concat "^\\* " (regexp-quote predicate) " */")))
    (require 'info)
    (pop-to-buffer nil)
    (Info-goto-node des-info-predicate-index)
    (if (not (re-search-forward str nil t))
        (error (format "Help on predicate `%s' not found." predicate)))

    (setq oldp (point))
    (if (re-search-forward str nil t)
        ;; Multiple matches, ask user
        (let ((max 2)
              n)
          ;; Count matches
          (while (re-search-forward str nil t)
            (setq max (1+ max)))

          (goto-char oldp)
          (re-search-backward "[^ /]" nil t)
          (recenter 0)
          (setq n (read-input 
                   (format "Several matches, choose (1-%d): " max) "1"))
          (forward-line (- (string-to-number n) 1)))
      ;; Single match
      (re-search-backward "[^ /]" nil t))

    ;; (Info-follow-nearest-node (point))
    (des-Info-follow-nearest-node)
    (re-search-forward (concat "^`" (regexp-quote predicate)) nil t)
    (beginning-of-line)
    (recenter 0)
    (pop-to-buffer buffer)))

(defun des-Info-follow-nearest-node ()
  (if (eq des-emacs 'xemacs)
	(Info-follow-nearest-node (point))
      (Info-follow-nearest-node))
)

(defun des-help-online (predicate)
  (des-ensure-process)
  (process-send-string "des" (concat "help(" predicate ").\n"))
  (display-buffer "*des*"))

(defun des-help-apropos (string)
  "Find Des apropos on given STRING.
This function is only available when `des-system' is set to `swi'."
  (interactive "sApropos: ")
  (cond
   ((eq des-system 'swi)
    (des-ensure-process)
    (process-send-string "des" (concat "apropos(" string ").\n"))
    (display-buffer "*des*"))
   (t
    (error "Sorry, no Des apropos available for this Des system."))))

(defun des-atom-under-point ()
  "Return the atom under or left to the point."
  (save-excursion
    (let ((nonatom_chars "[](){},\. \t\n")
          start)
      (skip-chars-forward (concat "^" nonatom_chars))
      (skip-chars-backward nonatom_chars)
      (skip-chars-backward (concat "^" nonatom_chars))
      (setq start (point))
      (skip-chars-forward (concat "^" nonatom_chars))
      (buffer-substring-no-properties start (point))
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help function with completion
;; Stolen from Per Mildner's SICStus debugger mode and modified

(defun des-find-documentation ()
  "Go to the Info node for a predicate in the SICStus Info manual."
  (interactive)
  (let ((pred (des-read-predicate)))
    (des-goto-predicate-info pred)))

(defvar des-info-alist nil 
  "Alist with all builtin predicates.
Only for internal use by `des-find-documentation'")

;; Very similar to des-help-info except that that function cannot
;; cope with arity and that it asks the user if there are several
;; functors with different arity. This function also uses
;; des-info-alist for finding the info node, rather than parsing
;; the predicate index.
(defun des-goto-predicate-info (predicate)
  "Go to the info page for PREDICATE, which is a PredSpec."
  (interactive)
  (require 'info)
  (string-match "\\(.*\\)/\\([0-9]+\\).*$" predicate)
  (let ((buffer (current-buffer))
        (name (match-string 1 predicate))
        (arity (match-string 2 predicate))
        ;oldp
        ;(str (regexp-quote predicate))
        )
    (setq arity (string-to-number arity))
    (pop-to-buffer nil)

    (Info-goto-node 
     des-info-predicate-index) ;; We must be in the SICStus pages
    (Info-goto-node (car (cdr (assoc predicate des-info-alist))))

    (des-find-term (regexp-quote name) arity "^`")

    (recenter 0)
    (pop-to-buffer buffer))
)

(defun des-read-predicate ()
  "Read a PredSpec from the user.
Returned value is a string \"FUNCTOR/ARITY\".
Interaction supports completion."
  (let ((initial (des-atom-under-point))
        answer)
    ;; If the predicate index is not yet built, do it now 
    (if (not des-info-alist) 
        (des-build-info-alist))
    ;; Test if the initial string could be the base for completion.
    ;; Discard it if not.
    (if (eq (try-completion initial des-info-alist) nil)
        (setq initial ""))
    ;; Read the PredSpec from the user
    (setq answer (completing-read 
                  "Help on predicate: "
                  des-info-alist nil t initial))
    (if (equal answer "")
        initial
      answer)))

(defun des-build-info-alist (&optional verbose)
  "Build an alist of all builtins and library predicates. 
Each element is of the form (\"NAME/ARITY\" . (INFO-NODE1 INFO-NODE2 ...)).
Typically there is just one Info node associated with each name
If an optional argument VERBOSE is non-nil, print messages at the beginning
and end of list building."
  (if verbose
      (message "Building info alist..."))
  (setq des-info-alist
        (let ((l ())
              (last-entry (cons "" ())))
          (save-excursion
            (save-window-excursion
              ;; select any window but the minibuffer (as we cannot switch
              ;; buffers in minibuffer window.
              ;; I am not sure this is the right/best way
              (if (active-minibuffer-window)  ; nil if none active
                  (select-window (next-window)))
              ;; Do this after going away from minibuffer window
              (save-window-excursion
                (info))
              (Info-goto-node des-info-predicate-index)
              (goto-char (point-min))
              (while (re-search-forward
                      "^\\* \\(.+\\)/\\([0-9]+\\)\\([^\n:*]*\\):" nil t)
                (let* ((name (match-string 1))
                       (arity (string-to-int (match-string 2)))
                       (comment (match-string 3))
                       (fa (format "%s/%d%s" name arity comment))
                       info-node)
                  (beginning-of-line)
                  ;; Extract the info node name
                  (setq info-node (progn 
                                    (re-search-forward ":[ \t]*\\([^:]+\\).$")
                                    (match-string 1)
                                   ))
                  ;; ###### Easier? (from Milan version 0.1.28)
                  ;; (setq info-node (Info-extract-menu-node-name))
                  (if (equal fa (car last-entry))
                      (setcdr last-entry (cons info-node (cdr last-entry)))
                    (setq last-entry (cons fa (list info-node))
                          l (cons last-entry l)))))
              (nreverse l)
              ))))
  (if verbose
      (message "Building info alist... done.")))


;;-------------------------------------------------------------------
;; Miscellaneous functions
;;-------------------------------------------------------------------

;; For Windows. Change backslash to slash. SICStus handles either
;; path separator but backslash must be doubled, therefore use slash.
(defun des-bsts (string)
  "Change backslashes to slashes in STRING."
  (let ((str1 (copy-sequence string))
        (len (length string))
        (i 0))
    (while (< i len)
      (if (char-equal (aref str1 i) ?\\)
          (aset str1 i ?/))
      (setq i (1+ i)))
    str1))

;(defun des-temporary-file ()
;  "Make temporary file name for compilation."
;  (make-temp-name 
;   (concat 
;    (or
;     (getenv "TMPDIR")
;     (getenv "TEMP") 
;     (getenv "TMP")
;     (getenv "SYSTEMP")
;     "/tmp")
;    "/prolcomp")))
;(setq des-temp-filename (des-bsts (des-temporary-file)))

(defun des-temporary-file ()
  "Make temporary file name for compilation."
  (if des-temporary-file-name
      ;; We already have a file, erase content and continue
      (progn
        (write-region "" nil des-temporary-file-name nil 'silent)
        des-temporary-file-name)
    ;; Actually create the file and set `des-temporary-file-name' accordingly
    (let* ((umask  (default-file-modes))
           (temporary-file-directory (or
                                      (getenv "TMPDIR")
                                      (getenv "TEMP") 
                                      (getenv "TMP")
                                      (getenv "SYSTEMP")
                                      "/tmp"))
           (prefix (expand-file-name "prolcomp" temporary-file-directory))
           (suffix ".pl")
           file)
      (unwind-protect
          (progn
            ;; Create temp files with strict access rights.
            (set-default-file-modes #o700)
            (while (condition-case ()
                       (progn
                         (setq file (concat (make-temp-name prefix) suffix))
                         ;; (concat (make-temp-name "/tmp/prolcomp") ".pl")
                         (unless (file-exists-p file)
                           (write-region "" nil file nil 'silent))
                         nil)
                     (file-already-exists t))
              ;; the file was somehow created by someone else between
              ;; `make-temp-name' and `write-region', let's try again.
              nil)
            (setq des-temporary-file-name file))
        ;; Reset the umask.
        (set-default-file-modes umask)))    
    ))

(defun des-goto-des-process-buffer ()
  "Switch to the des process buffer and go to its end."
  (switch-to-buffer-other-window "*des*")
  (goto-char (point-max))
)

(defun des-enable-sicstus-sd ()
  "Enable the source level debugging facilities of SICStus 3.7 and later."
  (interactive)
  (require 'pltrace) ; Load the SICStus debugger code
  ;; Turn on the source level debugging by default
  (add-hook 'des-inferior-mode-hook 'pltrace-on)
  (if (not des-use-sicstus-sd)
      (progn
        ;; If there is a *des* buffer, then call pltrace-on
        (if (get-buffer "*des*")
            ;; Avoid compilation warnings by using eval
            (eval '(pltrace-on)))
        (setq des-use-sicstus-sd t)
        ))
  )

(defun des-disable-sicstus-sd ()
  "Disable the source level debugging facilities of SICStus 3.7 and later."
  (interactive)
  (setq des-use-sicstus-sd nil)
  ;; Remove the hook
  (remove-hook 'des-inferior-mode-hook 'pltrace-on)
  ;; If there is a *des* buffer, then call pltrace-off
  (if (get-buffer "*des*")
      ;; Avoid compile warnings by using eval
      (eval '(pltrace-off))))

(defun des-debug-on (&optional arg)
  "Enable debugging.
When called with prefix argument ARG, disable debugging instead."
  (interactive "P")
  (if arg
      (des-debug-off)
    (des-process-insert-string (get-process "des")
                                  des-debug-on-string)
    (process-send-string "des" des-debug-on-string)))

(defun des-debug-off ()
  "Disable debugging."
  (interactive)
  (des-process-insert-string (get-process "des")
                                des-debug-off-string)
  (process-send-string "des" des-debug-off-string))

(defun des-trace-on (&optional arg)
  "Enable tracing.
When called with prefix argument ARG, disable tracing instead."
  (interactive "P")
  (if arg
      (des-trace-off)
    (des-process-insert-string (get-process "des")
                                  des-trace-on-string)
    (process-send-string "des" des-trace-on-string)))

(defun des-trace-off ()
  "Disable tracing."
  (interactive)
  (des-process-insert-string (get-process "des")
                                des-trace-off-string)
  (process-send-string "des" des-trace-off-string))

(defun des-zip-on (&optional arg)
  "Enable zipping (for SICStus 3.7 and later).
When called with prefix argument ARG, disable zipping instead."
  (interactive "P")
  (if arg
      (des-zip-off)
    (des-process-insert-string (get-process "des")
                                  des-zip-on-string)
    (process-send-string "des" des-zip-on-string)))

(defun des-zip-off ()
  "Disable zipping (for SICStus 3.7 and later)."
  (interactive)
  (des-process-insert-string (get-process "des")
                                des-zip-off-string)
  (process-send-string "des" des-zip-off-string))

;; (defun des-create-predicate-index ()
;;   "Create an index for all predicates in the buffer."
;;   (let ((predlist '())
;;         clauseinfo 
;;         object
;;         pos
;;         )
;;     (goto-char (point-min))
;;     ;; Replace with des-clause-start!
;;     (while (re-search-forward "^.+:-" nil t)
;;       (setq pos (match-beginning 0))
;;       (setq clauseinfo (des-clause-info))
;;       (setq object (des-in-object))
;;       (setq predlist (append
;;                       predlist
;;                       (list (cons 
;;                              (if (and (eq des-system 'sicstus)
;;                                       (des-in-object))
;;                                  (format "%s::%s/%d" 
;;                                          object
;;                                          (nth 0 clauseinfo) 
;;                                          (nth 1 clauseinfo))
;;                                (format "%s/%d"
;;                                        (nth 0 clauseinfo) 
;;                                        (nth 1 clauseinfo)))
;;                              pos
;;                              ))))
;;       (des-end-of-predicate))
;;     predlist))

(defun des-get-predspec ()
  (save-excursion
    (let ((state (des-clause-info))
          (object (des-in-object)))
      (if (equal (nth 0 state) "")
          nil
        (if (and (eq des-system 'sicstus)
                 object)
            (format "%s::%s/%d" 
                    object
                    (nth 0 state) 
                    (nth 1 state))
          (format "%s/%d"
                  (nth 0 state) 
                  (nth 1 state)))
        ))))

;; For backward compatibility. Stolen from custom.el.
(or (fboundp 'match-string)
    ;; Introduced in Emacs 19.29.
    (defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
          (substring string (match-beginning num) (match-end num))
        (buffer-substring (match-beginning num) (match-end num))))))

(defun des-pred-start ()
  "Return the starting point of the first clause of the current predicate."
  (save-excursion
    (goto-char (des-clause-start))
    ;; Find first clause, unless it was a directive
    (if (and (not (looking-at "[:?]-"))
             (not (looking-at "[ \t]*[%/]"))) ; Comment
        (let* ((pinfo (des-clause-info))
               (predname (nth 0 pinfo))
               (arity (nth 1 pinfo))
               (op (point)))
          (while (and (re-search-backward
                       (format "^%s\\([(\\.]\\| *%s\\)" 
                               predname des-head-delimiter) nil t)
                      (= arity (nth 1 (des-clause-info)))
                      )
            (setq op (point)))
          (if (eq des-system 'mercury)
              ;; Skip to the beginning of declarations of the predicate
              (progn
                (goto-char (des-beginning-of-clause))
                (while (and (not (eq (point) op))
                            (looking-at
                             (format ":-[ \t]*\\(pred\\|mode\\)[ \t]+%s"
                                     predname)))
                  (setq op (point))
                  (goto-char (des-beginning-of-clause)))))
          op)
      (point))))

(defun des-pred-end ()
  "Return the position at the end of the last clause of the current predicate."
  (save-excursion
    (goto-char (des-clause-end))        ; if we are before the first predicate
    (goto-char (des-clause-start))
    (let* ((pinfo (des-clause-info))
          (predname (nth 0 pinfo))
          (arity (nth 1 pinfo))
          oldp
          (notdone t)
          (op (point)))
      (if (looking-at "[:?]-")
          ;; This was a directive
          (progn
            (if (and (eq des-system 'mercury)
                     (looking-at
                      (format ":-[ \t]*\\(pred\\|mode\\)[ \t]+\\(%s+\\)"
                              des-atom-regexp)))
                ;; Skip predicate declarations
                (progn
                  (setq predname (buffer-substring-no-properties
                                  (match-beginning 2) (match-end 2)))
                  (while (re-search-forward
                          (format
                           "\n*\\(:-[ \t]*\\(pred\\|mode\\)[ \t]+\\)?%s[( \t]"
                           predname)
                          nil t))))
            (goto-char (des-clause-end))
            (setq op (point)))
        ;; It was not a directive, find the last clause
        (while (and notdone
                    (re-search-forward
                     (format "^%s\\([(\\.]\\| *%s\\)" 
                             predname des-head-delimiter) nil t)
                    (= arity (nth 1 (des-clause-info))))
          (setq oldp (point))
          (setq op (des-clause-end))
          (if (>= oldp op)
              ;; End of clause not found.
              (setq notdone nil)
            ;; Continue while loop
            (goto-char op))))
      op)))

(defun des-clause-start (&optional not-allow-methods)
  "Return the position at the start of the head of the current clause.
If NOTALLOWMETHODS is non-nil then do not match on methods in
objects (relevent only if 'des-system' is set to 'sicstus)."
  (save-excursion
    (let ((notdone t)
          (retval (point-min)))
      (end-of-line)
      
      ;; SICStus object?
      (if (and (not not-allow-methods)
               (eq des-system 'sicstus)
               (des-in-object))
          (while (and 
                  notdone 
                  ;; Search for a head or a fact
                  (re-search-backward
                   ;; If in object, then find method start.
                   ;; "^[ \t]+[a-z$].*\\(:-\\|&\\|:: {\\|,\\)" 
                   "^[ \t]+[a-z$].*\\(:-\\|&\\|:: {\\)" ; The comma causes
                                        ; problems since we cannot assume
                                        ; that the line starts at column 0,
                                        ; thus we don't know if the line
                                        ; is a head or a subgoal
                   (point-min) t))
            (if (>= (des-paren-balance) 0) ; To no match on "   a) :-"
                ;; Start of method found
                (progn
                  (setq retval (point))
                  (setq notdone nil)))
            )                                ; End of while

        ;; Not in object
        (while (and 
                notdone 
                ;; Search for a text at beginning of a line
                ;; ######
                ;; (re-search-backward "^[a-z$']" nil t))
                (re-search-backward
                 ;; (format "^[%s$']" des-lower-case-string)
                 (format "^\\([%s$']\\|[:?]-\\)" des-lower-case-string)
                 nil t))
          (let ((bal (des-paren-balance)))
            (cond
             ((> bal 0)
              ;; Start of clause found
              (progn
                (setq retval (point))
                (setq notdone nil)))
             ((and (= bal 0)
                   (looking-at
                    (format ".*\\(\\.\\|%s\\|!,\\)[ \t]*\\(%%.*\\|\\)$" 
                            des-head-delimiter)))
              ;; Start of clause found if the line ends with a '.' or
              ;; a des-head-delimiter
              (progn
                (setq retval (point))
                (setq notdone nil))
              )
             (t nil) ; Do nothing
             ))))
                
        retval)))

(defun des-clause-end (&optional not-allow-methods)
  "Return the position at the end of the current clause.
If NOTALLOWMETHODS is non-nil then do not match on methods in
objects (relevent only if 'des-system' is set to 'sicstus)."
  (save-excursion
    (beginning-of-line)                ; Necessary since we use "^...." for the search
    (if (re-search-forward 
         (if (and (not not-allow-methods)
                  (eq des-system 'sicstus)
                  (des-in-object))
             (format
              "^\\(%s\\|%s\\|[^\n\'\"%%]\\)*&[ \t]*\\(\\|%%.*\\)$\\|[ \t]*}"
              des-quoted-atom-regexp des-string-regexp)
           (format
            "^\\(%s\\|%s\\|[^\n\'\"%%]\\)*\\.[ \t]*\\(\\|%%.*\\)$"
            des-quoted-atom-regexp des-string-regexp))
         nil t)
        (if (and (des-in-string-or-comment)
                 (not (eobp)))
            (progn
              (forward-char)
              (des-clause-end))
          (point))
      (point))))

(defun des-clause-info ()
  "Return a (name arity) list for the current clause."
  (let (predname (arity 0))
    (save-excursion
      (goto-char (des-clause-start))
      (let ((op (point)))
        (if (looking-at des-atom-char-regexp)
            (progn
              (skip-chars-forward "^ (\\.")
              (setq predname (buffer-substring op (point))))
          (setq predname ""))
        ;; Retrieve the arity
        (if (looking-at des-left-paren)
            (let ((endp (save-excursion
                          (des-forward-list) (point))))
              (setq arity 1)
              (forward-char 1)                ; Skip the opening paren
              (while (progn
                       (skip-chars-forward "^[({,'\"")
                       (< (point) endp))
                (if (looking-at ",")
                    (progn
                      (setq arity (1+ arity))
                      (forward-char 1)        ; Skip the comma
                      )
                  ;; We found a string, list or something else we want
                  ;; to skip over. Always use des-tokenize,
                  ;; parse-partial-sexp does not have a 'skipover mode.
                  (goto-char (nth 5 (des-tokenize (point) endp 'skipover))))
                )))
        (list predname arity)
        ))))

(defun des-in-object ()
  "Return object name if the point is inside a SICStus object definition."
  ;; Return object name if the last line that starts with a character
  ;; that is neither white space nor a comment start
  (save-excursion
    (if (save-excursion 
          (beginning-of-line)
          (looking-at "\\([^\n ]+\\)[ \t]*::[ \t]*{"))
        ;; We were in the head of the object
        (match-string 1)
      ;; We were not in the head
      (if (and (re-search-backward "^[a-z$'}]" nil t)
               (looking-at "\\([^\n ]+\\)[ \t]*::[ \t]*{"))
          (match-string 1)
        nil))))

(defun des-forward-list ()
  "Move the point to the matching right parenthesis."
  (interactive)
  (if des-use-des-tokenizer-flag
      (let ((state (des-tokenize (point) (point-max) 'zerodepth)))
        (goto-char (nth 5 state)))
    (forward-list)))

;; NB: This could be done more efficiently!
(defun des-backward-list ()
  "Move the point to the matching left parenthesis."
  (interactive)
  (if des-use-des-tokenizer-flag
      (let ((bal 0)
            (paren-regexp (concat des-left-paren "\\|" des-right-paren))
            (notdone t))
        (while (and notdone (re-search-backward paren-regexp nil t))
          (cond
           ((looking-at des-left-paren)
            (if (not (des-in-string-or-comment))
                (setq bal (1+ bal)))
            (if (= bal 0)
                (setq notdone nil)))
           ((looking-at des-right-paren)
            (if (not (des-in-string-or-comment))
                (setq bal (1- bal))))
           )))
    (backward-list)))

(defun des-beginning-of-clause ()
  "Move to the beginning of current clause.
If already at the beginning of clause, move to previous clause."
  (interactive)
  (let ((point (point))
        (new-point (des-clause-start)))
    (if (and (>= new-point point)
             (> point 1))
        (progn
          (goto-char (1- point))
          (goto-char (des-clause-start)))
      (goto-char new-point)
      (skip-chars-forward " \t"))))

;; (defun des-previous-clause ()
;;   "Move to the beginning of the previous clause."
;;   (interactive)
;;   (forward-char -1)
;;   (des-beginning-of-clause))

(defun des-end-of-clause ()
  "Move to the end of clause.
If already at the end of clause, move to next clause."
  (interactive)
  (let ((point (point))
        (new-point (des-clause-end)))
    (if (and (<= new-point point)
             (not (eq new-point (point-max))))
        (progn
          (goto-char (1+ point))
          (goto-char (des-clause-end)))
      (goto-char new-point))))

;; (defun des-next-clause ()
;;   "Move to the beginning of the next clause."
;;   (interactive)
;;   (des-end-of-clause)
;;   (forward-char)
;;   (des-end-of-clause)
;;   (des-beginning-of-clause))

(defun des-beginning-of-predicate ()
  "Go to the nearest beginning of predicate before current point.
Return the final point or nil if no such a beginning was found."
  (interactive)
  (let ((op (point))
        (pos (des-pred-start)))
    (if pos
        (if (= op pos)
            (if (not (bobp))
                (progn
                  (goto-char pos)
                  (backward-char 1)
                  (setq pos (des-pred-start))
                  (if pos
                      (progn
                        (goto-char pos)
                        (point)))))
          (goto-char pos)
          (point)))))

(defun des-end-of-predicate ()
  "Go to the end of the current predicate."
  (interactive)
  (let ((op (point)))
    (goto-char (des-pred-end))
    (if (= op (point))
        (progn
          (forward-line 1)
          (des-end-of-predicate)))))

(defun des-insert-predspec ()
  "Insert the predspec for the current predicate."
  (interactive)
  (let* ((pinfo (des-clause-info))
         (predname (nth 0 pinfo))
         (arity (nth 1 pinfo)))
    (insert (format "%s/%d" predname arity))))

(defun des-view-predspec ()
  "Insert the predspec for the current predicate."
  (interactive)
  (let* ((pinfo (des-clause-info))
         (predname (nth 0 pinfo))
         (arity (nth 1 pinfo)))
    (message (format "%s/%d" predname arity))))

(defun des-insert-predicate-template ()
  "Insert the template for the current clause."
  (interactive)
  (let* ((n 1)
         oldp
         (pinfo (des-clause-info))
         (predname (nth 0 pinfo))
         (arity (nth 1 pinfo)))
    (insert predname)
    (if (> arity 0)
        (progn
          (insert "(")
	  (when des-electric-dot-full-predicate-template
	    (setq oldp (point))
	    (while (< n arity)
	      (insert ",")
	      (setq n (1+ n)))
	    (insert ")")
	    (goto-char oldp))
          ))
  ))

(defun des-insert-next-clause ()
  "Insert newline and the name of the current clause."
  (interactive)
  (insert "\n")
  (des-insert-predicate-template))

(defun des-insert-module-modeline ()
  "Insert a modeline for module specification.
This line should be first in the buffer.
The module name should be written manually just before the semi-colon."
  (interactive)
  (insert "%%% -*- Module: ; -*-\n")
  (backward-char 6))

(defun des-uncomment-region (beg end)
  "Uncomment the region between BEG and END."
  (interactive "r")
  (comment-region beg end -1))

(defun des-goto-comment-column (&optional nocreate)
  "Move comments on the current line to the correct position.
If NOCREATE is nil (or omitted) and there is no comment on the line, then
a new comment is created."
  (interactive)
  (beginning-of-line)
  (if (or (not nocreate)
          (and
           (re-search-forward 
            (format "^\\(\\(%s\\|%s\\|[^\n\'\"%%]\\)*\\)%% *" 
                    des-quoted-atom-regexp des-string-regexp)
            (save-excursion (end-of-line) (point)) 'limit)
           (progn
             (goto-char (match-beginning 0))
             (not (eq (des-in-string-or-comment) 'txt)))))
      (indent-for-comment)))

(defun des-indent-predicate ()
  "*Indent the current predicate."
  (interactive)
  (indent-region (des-pred-start) (des-pred-end) nil))

(defun des-indent-buffer ()
  "*Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun des-mark-clause ()
  "Put mark at the end of this clause and move point to the beginning."
  (interactive)
  (let ((pos (point)))
    (goto-char (des-clause-end))
    (forward-line 1)
    (beginning-of-line)
    (set-mark (point))
    (goto-char pos)
    (goto-char (des-clause-start))))

(defun des-mark-predicate ()
  "Put mark at the end of this predicate and move point to the beginning."
  (interactive)
  (let (pos)
    (goto-char (des-pred-end))
    (setq pos (point))
    (forward-line 1)
    (beginning-of-line)
    (set-mark (point))
    (goto-char pos)
    (goto-char (des-pred-start))))

;; Stolen from `cc-mode.el':
(defun des-electric-delete (arg)
  "Delete preceding character or whitespace.
If `des-hungry-delete-key-flag' is non-nil, then all preceding whitespace is
consumed.  If however an ARG is supplied, or `des-hungry-delete-key-flag' is
nil, or point is inside a literal then the function in the variable
`backward-delete-char' is called."
  (interactive "P")
  (if (or (not des-hungry-delete-key-flag)
          arg
          (des-in-string-or-comment))
      (funcall 'backward-delete-char (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
          (delete-region (point) here)
        (funcall 'backward-delete-char 1)
        ))))

;; For XEmacs compatibility (suggested by Per Mildner)
(put 'des-electric-delete 'pending-delete 'supersede)

(defun des-electric-if-then-else (arg)
  "If `des-electric-if-then-else-flag' is non-nil, indent
if-then-else constructs. Bound to the >, ; and ( keys."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if des-electric-if-then-else-flag (des-insert-spaces-after-paren)))

(defun des-electric-colon (arg)
  "If `des-electric-colon-flag' is non-nil, insert space (if
appropriate), `:-' and newline if colon is pressed at the end of
a line that starts in the first column (i.e., clause heads)."
  (interactive "P")
  (if (and des-electric-colon-flag
	   (null arg)
	   (= (point) (line-end-position))
	   (not (= (point) (line-beginning-position)))
	   (not (string-match "^\\(\\s \\:\\|%\\)" (thing-at-point 'line))))
      (progn
	(unless (looking-back "\\s ") (insert " "))
	(insert ":-\n")
	(des-indent-line))
    (self-insert-command (prefix-numeric-value arg))))

(defun des-electric-dash (arg)
  "If `des-electric-dash-flag' is non-nil, insert space (if
appropriate), `-->' and newline if dash is pressed at the end of
a line that starts in the first column (i.e., DCG heads)."
  (interactive "P")
  (if (and des-electric-dash-flag
	   (null arg)
	   (= (point) (line-end-position))
	   (not (= (point) (line-beginning-position)))
	   (not (string-match "^\\(\\s \\|:\\|%\\)" (thing-at-point 'line))))
      (progn
	(unless (looking-back "\\s ") (insert " "))
	(insert "-->\n")
	(des-indent-line))
    (self-insert-command (prefix-numeric-value arg))))

(defun des-electric-dot (arg)
  "Insert dot and newline or a head of a new clause.

If `des-electric-dot-flag' is nil, then simply insert dot.
Otherwise::
When invoked at the end of nonempty line, insert dot and newline.
When invoked at the end of an empty line, insert a recursive call to
the current predicate.
When invoked at the beginning of line, insert a head of a new clause
of the current predicate.

When called with prefix argument ARG, insert just dot."
  (interactive "P")
  ;; Check for situations when the electricity should not be active
  (if (or (not des-electric-dot-flag)
          arg
          (des-in-string-or-comment)
          ;; Do not be electric in a floating point number or an operator
          (not 
           (or
            ;; (re-search-backward
            ;; ######
            ;; "\\(^\\|[])}a-zA-Z_!'0-9]+\\)[ \t]*\\=" nil t)))
            (save-excursion 
              (re-search-backward
               ;; "\\(^\\|[])}_!'0-9]+\\)[ \t]*\\=" nil t)))
               "\\(^\\|[])}_!'0-9]+\\)[ \t]*\\=" 
               nil t))
            (save-excursion 
              (re-search-backward
               ;; "\\(^\\|[])}a-zA-Z]+\\)[ \t]*\\=" nil t)))
               (format "\\(^\\|[])}%s]+\\)[ \t]*\\=" 
                       des-lower-case-string)
               nil t))
              (save-excursion 
              (re-search-backward
               ;; "\\(^\\|[])}a-zA-Z]+\\)[ \t]*\\=" nil t)))
               (format "\\(^\\|[])}%s]+\\)[ \t]*\\=" 
                       des-upper-case-string)
               nil t))
             )
            )
          ;; Do not be electric if inside a parenthesis pair.
          (not (= (des-region-paren-balance (des-clause-start) (point))
                  0))
          )
      (funcall 'self-insert-command (prefix-numeric-value arg))
    (cond
     ;; Beginning of line
     ((bolp)
      (des-insert-predicate-template))
     ;; At an empty line with at least one whitespace
     ((save-excursion
        (beginning-of-line)
        (looking-at "[ \t]+$"))
      (des-insert-predicate-template)
      (when des-electric-dot-full-predicate-template
	(save-excursion 
	  (end-of-line)
	  (insert ".\n"))))
     ;; Default
     (t
	(insert ".")
	(when des-electric-dot-full-predicate-template
	  (insert "\n")))
     )))

(defun des-electric-underscore ()
  "Replace variable with an underscore.
If `des-electric-underscore-flag' is non-nil and the point is
on a variable then replace the variable with underscore and skip
the following comma and whitespace, if any.
If the point is not on a variable then insert underscore."
  (interactive)
  (if des-electric-underscore-flag
      (let (;start
            (oldcase case-fold-search)
            (oldp (point)))
        (setq case-fold-search nil)
        ;; ######
        ;;(skip-chars-backward "a-zA-Z_")
        (skip-chars-backward
         (format "%s%s_" 
                 des-lower-case-string 
                 des-upper-case-string))

        ;(setq start (point))
        (if (and (not (des-in-string-or-comment))
                 ;; ######
                 ;; (looking-at "\\<[_A-Z][a-zA-Z_0-9]*\\>"))
                 (looking-at (format "\\<[_%s][%s%s_0-9]*\\>" 
                                     des-upper-case-string
                                     des-lower-case-string
                                     des-upper-case-string)))
            (progn
              (replace-match "_")
              (skip-chars-forward ", \t\n"))
          (goto-char oldp)
          (self-insert-command 1))
        (setq case-fold-search oldcase)
        )
    (self-insert-command 1))
  )


(defun des-find-term (functor arity &optional prefix)
  "Go to the position at the start of the next occurance of a term.
The term is specified with FUNCTOR and ARITY. The optional argument
PREFIX is the prefix of the search regexp."
  (let* (;; If prefix is not set then use the default "\\<"
         (prefix (if (not prefix)
                     "\\<"
                   prefix))
         (regexp (concat prefix functor))
         (i 1))
      
    ;; Build regexp for the search if the arity is > 0
    (if (= arity 0)
        ;; Add that the functor must be at the end of a word. This
        ;; does not work if the arity is > 0 since the closing )
        ;; is not a word constituent.
        (setq regexp (concat regexp "\\>"))
      ;; Arity is > 0, add parens and commas
      (setq regexp (concat regexp "("))
      (while (< i arity)
        (setq regexp (concat regexp ".+,"))
        (setq i (1+ i)))
      (setq regexp (concat regexp ".+)")))
      
    ;; Search, and return position
    (if (re-search-forward regexp nil t)
        (goto-char (match-beginning 0))
      (error "Term not found"))
    ))

(defun des-variables-to-anonymous (beg end)
  "Replace all variables within a region BEG to END by anonymous variables."
  (interactive "r")
  (save-excursion
    (let ((oldcase case-fold-search))
      (setq case-fold-search nil)
      (goto-char end)
      (while (re-search-backward "\\<[A-Z_][a-zA-Z_0-9]*\\>" beg t)
        (progn
          (replace-match "_")
          (backward-char)))
      (setq case-fold-search oldcase)
      )))


(defun des-set-atom-regexps ()
  "Set the `des-atom-char-regexp' and `des-atom-regexp' variables.
Must be called after `des-build-case-strings'."
  (setq des-atom-char-regexp
        (format "[%s%s0-9_$]" 
                des-lower-case-string 
                des-upper-case-string))
  (setq des-atom-regexp
        (format "[%s$]%s*" 
                des-lower-case-string 
                des-atom-char-regexp))
  )

(defun des-build-case-strings ()
  "Set `des-upper-case-string' and `des-lower-case-string'.
Uses the current case-table for extracting the relevant information."
  (let ((up_string "")
        (low_string ""))
    ;; Use `map-char-table' if it is defined. Otherwise enumerate all
    ;; numbers between 0 and 255. `map-char-table' is probably safer.
    ;;
    ;; `map-char-table' causes problems under Emacs 23.0.0.1, the 
    ;; while loop seems to do its job well (Ryszard Szopa)
    ;; 
    ;;(if (and (not (eq des-emacs 'xemacs))
    ;;          (fboundp 'map-char-table))
    ;;    (map-char-table
    ;;     (lambda (key value)
    ;;       (cond 
    ;;        ((and 
    ;;          (eq (int-to-char key) (downcase key))
    ;;          (eq (int-to-char key) (upcase key)))
    ;;         ;; Do nothing if upper and lower case are the same
    ;;         )
    ;;        ((eq (int-to-char key) (downcase key))
    ;;         ;; The char is lower case
    ;;         (setq low_string (format "%s%c" low_string key)))
    ;;        ((eq (int-to-char key) (upcase key))
    ;;         ;; The char is upper case
    ;;         (setq up_string (format "%s%c" up_string key)))
    ;;        ))
    ;;     (current-case-table))
      ;; `map-char-table' was undefined.
      (let ((key 0))
        (while (< key 256)
          (cond 
           ((and 
             (eq (int-to-char key) (downcase key))
             (eq (int-to-char key) (upcase key)))
            ;; Do nothing if upper and lower case are the same
            )
           ((eq (int-to-char key) (downcase key))
            ;; The char is lower case
            (setq low_string (format "%s%c" low_string key)))
           ((eq (int-to-char key) (upcase key))
            ;; The char is upper case
            (setq up_string (format "%s%c" up_string key)))
           )
          (setq key (1+ key))))
      ;; )
      ;; The strings are single-byte strings
      (setq des-upper-case-string (des-dash-letters up_string))
      (setq des-lower-case-string (des-dash-letters low_string))
      ))

;(defun des-regexp-dash-continuous-chars (chars)
;  (let ((ints (mapcar #'char-to-int (string-to-list chars)))
;        (beg 0)
;        (end 0))
;    (if (null ints)
;        chars
;      (while (and (< (+ beg 1) (length chars))
;                  (not (or (= (+ (nth beg ints) 1) (nth (+ beg 1) ints))
;                           (= (nth beg ints) (nth (+ beg 1) ints)))))
;        (setq beg (+ beg 1)))
;      (setq beg (+ beg 1)
;            end beg)
;      (while (and (< (+ end 1) (length chars))
;                  (or (= (+ (nth end ints) 1) (nth (+ end 1) ints))
;                      (= (nth end ints) (nth (+ end 1) ints))))
;        (setq end (+ end 1)))
;      (if (equal (substring chars end) "")
;          (substring chars 0 beg)
;        (concat (substring chars 0 beg) "-" 
;                (des-regexp-dash-continuous-chars (substring chars end))))
;    )))

(defun des-ints-intervals (ints)
  "Return a list of intervals (from . to) covering INTS."
  (when ints
    (setq ints (sort ints '<))
    (let ((prev (car ints))
	  (interval-start (car ints))
	  intervals)
      (while ints
	(let ((next (car ints)))
	  (when (> next (1+ prev))	; start of new interval
	      (setq intervals (cons (cons interval-start prev) intervals))
	      (setq interval-start next))
	  (setq prev next)
	  (setq ints (cdr ints))))
      (setq intervals (cons (cons interval-start prev) intervals))
      (reverse intervals))))

(defun des-dash-letters (string)
  "Return a condensed regexp covering all letters in STRING."
  (let ((intervals (des-ints-intervals (mapcar #'char-to-int
						 (string-to-list string))))
	codes)
    (while intervals
      (let* ((i (car intervals))
	     (from (car i))
	     (to (cdr i))
	     (c (cond ((= from to) `(,from))
		      ((= (1+ from) to) `(,from ,to))
		      (t `(,from ?- ,to)))))
	(setq codes (cons c codes)))
      (setq intervals (cdr intervals)))
    (apply 'concat (reverse codes))))

;(defun des-condense-character-sets (regexp)
;  "Condense adjacent characters in character sets of REGEXP."
;  (let ((next -1))
;    (while (setq next (string-match "\\[\\(.*?\\)\\]" regexp (1+ next)))
;      (setq regexp (replace-match (des-dash-letters (match-string 1 regexp))
;				  t t regexp 1))))
;  regexp)

;; GNU Emacs compatibility: GNU Emacs does not differentiate between
;; ints and chars, or at least these two are interchangeable.
(or (fboundp 'int-to-char)
    ;; Introduced in Emacs 19.29.
    (defun int-to-char (num)
      num))

(or (fboundp 'char-to-int)
    ;; Introduced in Emacs 19.29.
    (defun char-to-int (num)
      num))


;;-------------------------------------------------------------------
;; Menu stuff (both for the editing buffer and for the inferior
;; des buffer)
;;-------------------------------------------------------------------

(unless (fboundp 'region-exists-p)
  (defun region-exists-p ()
    "Non-nil iff the mark is set.  Lobotomized version for Emacsen that do not provide their own."
    (mark)))

(defun des-menu ()
  "Creates the menus for the Des editing buffers.
These menus are dynamically created because one may change systems
during the life of an Emacs session, and because GNU Emacs wants them
so by ignoring `easy-menu-add'."

  ;; GNU Emacs ignores `easy-menu-add' so the order in which the menus
  ;; are defined _is_ important!

  (easy-menu-define 
   des-edit-menu-help (current-local-map)
   "Help menu for the Des mode."
   (append
    (if (eq des-emacs 'xemacs) '("Help") '("DES-help"))
    (cond
     ((eq des-system 'sicstus)
      '(["On predicate" des-help-on-predicate t]
        "---"))
     ((eq des-system 'swi)
      '(["On predicate" des-help-on-predicate t]
        ["Apropos" des-help-apropos t]
        "---")))
    '(["Describe mode" describe-mode t])))

  (easy-menu-define 
   des-edit-menu-runtime (current-local-map) 
   "Runtime Des commands available from the editing buffer"
   (append 
    ;; runtime menu name
    (list (cond ((eq des-system 'eclipse) 
                 "ECLiPSe")
                ((eq des-system 'mercury) 
                 "Mercury")
                (t
                 "DES")))
    ;; consult items, NIL for mercury
    (unless (eq des-system 'mercury)
      '("---"
        ["Consult file" des-consult t]
        ["Consult buffer" des-consult t]
        ["Consult region" des-consult (region-exists-p)]
        ))
    ;; compile items, NIL for everything but SICSTUS
    (when (eq des-system 'sicstus)
      '("---"
        ["Compile file" des-compile-file t]
        ["Compile buffer" des-compile-buffer t]
        ["Compile region" des-compile-region (region-exists-p)]
        ["Compile predicate" des-compile-predicate t]
        ))
    ;; debug items, NIL for mercury
    (cond 
     ((eq des-system 'sicstus) 
      ;; In SICStus, these are pairwise disjunctive,
      ;; so it's enough with one "off"-command
      (if (des-atleast-version '(3 . 7))
          (list "---"
                ["Debug" des-debug-on t]
                ["Trace" des-trace-on t]
                ["Zip" des-zip-on t]
                ["All debug off" des-debug-off t]
                '("Source level debugging"
                  ["Enable" des-enable-sicstus-sd t]
                  ["Disable" des-disable-sicstus-sd t]))
        (list "---"
              ["Debug" des-debug-on t]
              ["Trace" des-trace-on t]
              ["All debug off" des-debug-off t])))
     ;; default (mercury) nil
     )
    ))

  (easy-menu-define 
    des-edit-menu-insert-move (current-local-map) 
    "Commands for Des code manipulation."
    (append
     (list "Code"
           ["Comment region" comment-region (region-exists-p)]
           ["Uncomment region" des-uncomment-region (region-exists-p)]
           ["Add comment/move to comment" indent-for-comment t])
     (unless (eq des-system 'mercury)
       (list ["Convert variables in region to '_'" des-variables-to-anonymous (region-exists-p)]))
     (list "---"
           ["Insert predicate template" des-insert-predicate-template t]
           ["Insert next clause head" des-insert-next-clause t]
           ["Insert predicate spec" des-insert-predspec t]
           ["Insert module modeline" des-insert-module-modeline t]
           "---"
           ["Beginning of clause" des-beginning-of-clause t]
           ["End of clause" des-end-of-clause t]
           ["Beginning of predicate" des-beginning-of-predicate t]
           ["End of predicate" des-end-of-predicate t]
           "---"
           ["Indent line" des-indent-line t]
           ["Indent region" indent-region (region-exists-p)]
           ["Indent predicate" des-indent-predicate t]
           ["Indent buffer" des-indent-buffer t]
           ["Align region" align (region-exists-p)]
           "---"
           ["Mark clause" des-mark-clause t]
           ["Mark predicate" des-mark-predicate t]
           ["Mark paragraph" mark-paragraph t]
           ;"---"
           ;["Fontify buffer" font-lock-fontify-buffer t]
           )))

  (easy-menu-add des-edit-menu-insert-move)
  (easy-menu-add des-edit-menu-runtime)
        
  ;; Add predicate index menu
  (make-variable-buffer-local 'imenu-create-index-function)
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  ;;Milan (this has problems with object methods...)  ###### Does it? (Stefan)
  (setq imenu-prev-index-position-function 'des-beginning-of-predicate)
  (setq imenu-extract-index-name-function 'des-get-predspec)
  
  (if (and des-imenu-flag
           (< (count-lines (point-min) (point-max)) des-imenu-max-lines))
      (imenu-add-to-menubar "Predicates"))
  
  (easy-menu-add des-edit-menu-help))


(add-hook 'des-mode-hook 'des-menu)

(add-hook 'des-mode-hook '(lambda () (font-lock-mode 1)))
(add-hook 'des-inferior-mode-hook '(lambda () (font-lock-mode 1)))


(defun des-mode-version ()
  "Echo the current version of Des mode in the minibuffer."
  (interactive)
  (message "Using Des mode version %s" des-mode-version))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom des-prolog-file "~/des/des.pl"
  "Path to the des.pl Prolog source file."
  :group 'des
  :type 'string)

(defvar des-temp-file nil)

(defun des-consult ()
  "Consults the current buffer into DES.
In transient mark mode, if the region is active, consults the
region."
  (interactive)
  (if des-temp-file
      (write-region "" nil des-temp-file nil 'silent)
    (setq des-temp-file (make-temp-file "des")))
  (let ((start (if (and transient-mark-mode mark-active)
		   (region-beginning) (point-min)))
	(end (if (and transient-mark-mode mark-active)
		 (region-end) (point-max))))
    (write-region start end des-temp-file nil 'silent)
    (make-comint "des" "pl" nil "-s" (expand-file-name des-prolog-file))
    (switch-to-buffer-other-window "*des*")
    (local-set-key "\t" 'comint-dynamic-complete)
    (add-to-list 'comint-dynamic-complete-functions
		 'des-complete)
    (comint-send-string (get-process "des")
			(concat "/c " des-temp-file "\n"))))


(defvar des-commands
  '("/consult" "/reconsult" "/assert" "/retract" "/retractall" "/abolish"
    "/listing" "/list_et" "/clear_et" "/cd" "/pwd" "/ls" "/dir" "/shell"
    "/log" "/nolog" "/help" "/builtins" "/verbose" "/noverbose" "/strata"
    "/pdg" "/version" "/prolog" "/halt"))


(defun des-complete ()
  (if (looking-back "/[a-z_]*")
      (or (comint-dynamic-simple-complete (match-string 0) des-commands))
    (error "Type /help for commands")))


(provide 'des)

;;; des.el ends here
