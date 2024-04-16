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

(defun arei--get-completion-candidate (dict)
  (arei-nrepl-dict-get dict "candidate"))

(defun arei-complete-at-point ()
  "Function to be used for the hook `completion-at-point-functions'."
  (interactive)
  (when (arei-connected-p)
    (let* ((bnds (bounds-of-thing-at-point 'symbol))
           (start (car bnds))
           (end (cdr bnds))
           ;; (ns (monroe-get-clojure-ns))
           (sym (thing-at-point 'symbol))
           (request (arei-nrepl-dict "op" "completions"
                                     "prefix" sym))
           (module (arei-current-module))
           (_ (when module
                (arei-nrepl-dict-put request "ns" module)))
           (response (arei-send-sync-request request)))
      (when-let* ((completions (arei-nrepl-dict-get response "completions")))
        (list start end
              (mapcar 'arei--get-completion-candidate completions)
              nil)))))

(provide 'arei-completion)
;;; arei-completion.el ends here
