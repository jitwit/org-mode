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
	 (sessionp (cdr (assq :session params)))
	 (sit-time (let ((sit (assq :sit params)))
		     (if sit (cdr sit) .1)))
         (full-body (org-babel-expand-body:J
                     body params processed-params)))
    ;; (org-babel-j-initiate-session sessionp)
    (j-do WWJ (concat "1!:44 '" default-directory "'"))
    (with-temp-buffer
      (j-eval* WWJ body)
      (buffer-string))))

(defun org-babel-j-initiate-session (&optional session)
  "Initiate a J session.
SESSION is a parameter given by org-babel."
  (unless (string= session "none")
    (j-create-instance (buffer-file-name))))

(provide 'ob-J)

;;; ob-J.el ends here
