;;; ob-J.el --- Babel Functions for J                -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2020 Free Software Foundation, Inc.

;; Author: Oleh Krehel
;; Maintainer: Joseph Novakovich <josephnovakovich@gmail.com>
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating J code.
;;
;; Session interaction depends on `j-console' from package `j-mode'
;; (available in MELPA).

;;; Code:

(require 'ob)
(require 'org-macs)

;; (declare-function j-console-ensure-session "ext:j-console" ())

(defcustom org-babel-J-command "jconsole"
  "Command to call J."
  :group 'org-babel
  :version "26.1"
  :package-version '(Org . "9.0")
  :type 'string)

(defun org-babel-expand-body:J (body _params &optional _processed-params)
  "Expand BODY according to PARAMS, return the expanded body.
PROCESSED-PARAMS isn't used yet."
  body)

(defalias 'org-babel-execute:j 'org-babel-execute:J)

(defun org-babel-execute:J (body params)
  "Execute a block of J code BODY.
PARAMS are given by org-babel.
This function is called by `org-babel-execute-src-block'."
  (message "executing J source code block")
  (let* ((processed-params (org-babel-process-params params))
	 ;; no session => global, session => file local, 
	 (session-id (let ((session (assq :session params)))
		       (if (equal "none" (cdr session))
			   "~"
			 (buffer-file-name))))
	 ;; not needed with jpl-mode
	 (sit-time (let ((sit (assq :sit params)))
		     (if sit (cdr sit) .1)))
	 (foreign-verb (let ((verb (assq :verb params)))
			 (if verb (cdr verb) "0!:0")))
	 (plot (let ((plot (assq :plot params)))
		 (if plot (cdr plot) nil)))
         (full-body (org-babel-expand-body:J body params processed-params))
	 (J (org-babel-j-session session-id)))
    (cond (plot
	   (j-getr J (concat "1!:44 '" default-directory "'"))
	   (j-eval J body foreign-verb)
	   (j-save-plot (concat default-directory plot))
	   plot ;; (concat "[[file:" plot "]]")
	   )
	  (t
	   (j-getr J (concat "1!:44 '" default-directory "'"))
	   (j-eval J body foreign-verb)))))

(defun org-babel-j-session (session-id)
  "Get the given session's J instance, creating it if necessary.
SESSION is a parameter given by org-babel."
  (j-create-instance session-id)
  (cdr (assq 'engine (gethash session-id jpl-place->j))))

(provide 'ob-J)

;;; ob-J.el ends here
