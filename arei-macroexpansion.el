;;; arei-macroexpansion.el --- Macroexpansion for Arei  -*- lexical-binding: t; -*-

;; Copyright © 2024 Andrew Tropin <andrew@trop.in>

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

;; Macroexpansion for Arei

;;; Code:

(require 'arei-client)
(require 'arei-syntax)
(require 'arei-nrepl)

(eval-when-compile (require 'map))
(eval-when-compile (require 'pcase))

(defun arei--process-guile-macroexpand-response-callback
    (connection-buffer &optional expression-end)
  "Set up a handler for macroexpand request responses."
  (lambda (response)
    (pcase response
      ((map status expansion error)
       (goto-char (point-max))
       (when error
         (insert (propertize error 'face
                             '((t (:inherit font-lock-warning-face))))))
       (when expansion
         (unless (= 0 (current-column))
           (insert "\n"))
         (insert (propertize expansion 'face
                             '((t (:inherit font-lock-string-face)))))
         (insert "\n"))
       (when (member "done" status)
         (with-current-buffer connection-buffer
           (let* ((fmt (if expansion " -> %s" " ;; interrupted"))
                  (forward-sexp-function
                   (lambda (&rest args)
                     ;; see https://github.com/xiongtx/eros/issues/10
                     (ignore-errors (apply #'forward-sexp args)))))
             (unless (eros--make-result-overlay (or expansion "") ; response
                       :format fmt
                       :where (or expression-end (point))
                       :duration eros-eval-result-duration)
               (message fmt expansion))))
         ;; NOTE: stop spinner if it's the last request (we can have
         ;; multiple evals queued)
         (when (= 1 (hash-table-count arei-client--pending-requests))
           (arei-spinner-stop)))

       (when (get-buffer-window)
         (set-window-point (get-buffer-window) (buffer-size)))))))

(defun arei--request-guile-macroexpand (code &optional bounds)
  (pcase-let* ((`(,start . ,end) bounds)
               (code (or code
                         (buffer-substring-no-properties start end)))
               (request (arei-nrepl-dict
                         "op" "ares.guile.macroexpansion/macroexpand"
                         "code" code)))
    (when-let* ((module (arei-current-module)))
      (arei-nrepl-dict-put request "module" module))
    (arei-send-request
     request
     (arei-connection-buffer)
     (arei--process-guile-macroexpand-response-callback (current-buffer) end)
     t)
    (ignore-errors (arei-spinner-start))))

(defun arei-macroexpand-last-sexp ()
  (interactive)
  (arei--request-guile-macroexpand nil (arei-syntax-last-sexp-bounds)))

(defvar-keymap arei-macroexpansion-map
  "C-m" #'arei-macroexpand-last-sexp)

(provide 'arei-macroexpansion)
;;; arei-macroexpansion.el ends here
