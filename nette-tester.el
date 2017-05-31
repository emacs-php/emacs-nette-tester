;;; nette-tester.el --- Emacs support for Nette Tester -*- lexical-binding: t -*-

;; Copyright (C) 2017 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 1.0.0
;; Created: 30th May 2017
;; Package-requires: ((dash 2.13.0))
;; Keywords: convenience, languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dash)

(require 'compile)
(require 'diff)
(require 'ediff)

(defvar nette-tester-compilation-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'nette-tester-ediff)
    (define-key map [mouse-2] 'nette-tester-ediff)
    map)
  "Map used in compilation mode for additional nette tester actions.")

;;;###autoload
(defun nette-tester-ediff ()
  "Run `ediff' on the files on current line."
  (interactive)
  (-let (((_ file-a file-b) (split-string (thing-at-point 'line) "'" t " +")))
    (ediff-files file-a file-b)))

;;;###autoload
(defun nette-tester-diff ()
  "Run `diff' on the files on current line."
  (interactive)
  (-let (((_ file-a file-b) (split-string (thing-at-point 'line) "'" t " +")))
    (diff file-a file-b)))

;;;###autoload
(let ((form `(nette-tester
              ,(rx-to-string
                '(and "-- FAILED: " (* not-newline) 10
                      (and (*? (* not-newline) 10)
                           (? (and "   " (group "diff" (*? not-newline)) 10)
                              (*? (* not-newline) 10))
                           (and (? bol "  " (*? not-newline) " in ")
                                (group (*? not-newline) ".phpt")
                                (or "(" " on line ")
                                (group (1+ digit))
                                (? ")")))))
              2 3 nil 2 2 (1 (list 'face 'underline
                                   'keymap nette-tester-compilation-keymap
                                   'mouse-face 'highlight)))))
  (if (assq 'nette-tester compilation-error-regexp-alist-alist)
      (setf (cdr (assq 'nette-tester compilation-error-regexp-alist-alist)) form)
    (push form compilation-error-regexp-alist-alist)))

(provide 'nette-tester)
;;; nette-tester.el ends here
