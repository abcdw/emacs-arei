;;; arei-xref.el --- AREI's backend for xref -*- lexical-binding: t; -*-

;; Copyright © 2023, 2024 Andrew Tropin <andrew@trop.in>
;; Copyright © 2024 Nikita Domnitskii

;; Author: Andrew Tropin <andrew@trop.in>
;;         Nikita Domnitskii <nikita@domnitskii.me>

;; Keywords: languages, guile, scheme, nrepl

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

;; AREI's backend for xref

;;; Code:

(require 'arei-nrepl)
(require 'arei-client)
(require 'xref)

(defun arei--xref-backend () 'arei)

(defun arei-xref--request-lookup (sym)
  (let ((request (arei-nrepl-dict
                  "op" "lookup"
                  "sym" sym)))
    (when-let* ((module (arei-current-module)))
      (arei-nrepl-dict-put request "ns" module))
    (arei-nrepl-dict-get (arei-send-sync-request request) "info")))

(defun arei-xref--make-xref-loc (identifier)
  (when-let* ((info (arei-xref--request-lookup identifier))
              (file (arei-nrepl-dict-get info "file"))
              (line (arei-nrepl-dict-get info "line"))
              (column (arei-nrepl-dict-get info "column"))
              (buffer (find-file-noselect file)))
    (xref-make-buffer-location
     buffer
     (with-current-buffer buffer
       (save-excursion
         (goto-char 0)
         (forward-line (1- line))
         (move-to-column column)
         (point))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql arei)))
  "Return the relevant identifier at point."
  (when-let* ((thing (thing-at-point 'symbol)))
    (substring-no-properties thing)))

(cl-defmethod xref-backend-definitions ((_backend (eql arei)) identifier)
  (when (arei-connected-p)
    (when-let* ((loc (arei-xref--make-xref-loc identifier)))
      (list (xref-make identifier loc)))))

(provide 'arei-xref)
;;; arei-xref.el ends here
