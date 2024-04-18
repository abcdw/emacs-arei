;;; arei-completion.el --- Code completion for AREI  -*- lexical-binding: t; -*-

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

;; Code completion for AREI

;;; Code:

(require 'arei-client)
(require 'arei-nrepl)
(eval-when-compile (require 'map))
(eval-when-compile (require 'pcase))

(defun arei--get-completion-candidate (dict)
  (let ((candidate (arei-nrepl-dict-get dict "candidate")))
    (put-text-property 0 1 'completion-data dict candidate)
    candidate))

(defun arei--annotate-symbol (symbol)
  (pcase (get-text-property 0 'completion-data symbol)
    ((map ns type)
     (concat
      (when ns (format " %s" ns))
      (pcase type
        ("function" " <f>")
        ("macro" " <m>")
        ("var" " <v>"))))))

(defun arei-complete-at-point ()
  "Function to be used for the hook `completion-at-point-functions'."
  (interactive)
  (when (arei-connected-p)
    (let* ((bnds (bounds-of-thing-at-point 'symbol))
           (start (car bnds))
           (end (cdr bnds))
           (sym (thing-at-point 'symbol))
           (request (arei-nrepl-dict
                     "op" "completions"
                     "prefix" sym))
           (_ (when-let* ((module (arei-current-module)))
                (arei-nrepl-dict-put request "ns" module)))
           (response (arei-send-sync-request request)))
      (when-let* ((completions (arei-nrepl-dict-get response "completions")))
        (list start end
              (mapcar 'arei--get-completion-candidate completions)
              :annotation-function #'arei--annotate-symbol)))))

(provide 'arei-completion)
;;; arei-completion.el ends here
