;;; arei-syntax.el --- Tools for navigating and manipulating code -*- lexical-binding: t; coding:utf-8 -*-

;; Copyright © 2023, 2024 Andrew Tropin

;; Author: Andrew Tropin <andrew@trop.in>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tools for navigating and manipulating code

;;; Code:

(defconst arei-syntax--module-re
  "^[[:blank:]]*([[:blank:]\n]*define-module[[:blank:]\n]+\\(([^)]+)\\)"
  "Regular expression matching a top-level definition of the module.")

(defconst arei-syntax--library-re
  "^[[:blank:]]*([[:blank:]\n]*\\(?:define-\\)?library[[:blank:]\n]+\\(([^)]+)\\)"
  "Regular expression matching a top-level definition of the library.")

(defun arei-current-module ()
  "Find current buffer's module.  It goes backwards and looking for
`arei--module-re' or `arei--library-re' to match, after that it
goes the opposite direction.  This logic is not perfect, but
works good enough for this amount of code.  Also, it's similar
to geiser and cider approaches.  In the future it would be better
to extract this information from tree-sitter or some other more
preciese way."
  (save-excursion
    (when (or (re-search-backward arei-syntax--module-re nil t)
              (re-search-backward arei-syntax--library-re nil t)
              (re-search-forward arei-syntax--module-re nil t)
              (re-search-forward arei-syntax--library-re nil t))
      (match-string-no-properties 1))))

(provide 'arei-syntax)
