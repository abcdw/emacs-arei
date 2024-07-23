;;; arei-module.el --- Module Operations for Arei  -*- lexical-binding: t; -*-

;; Copyright © 2023, 2024 Andrew Tropin <andrew@trop.in>
;; Copyright © 2024 Nikita Domnitskii

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

;; Module Operations for Arei

;;; Code:

(require 'arei-evaluation)
(require 'arei-syntax)

(defun arei--get-modules ()
  (read (arei--get-expression-value
         "(map (lambda (m) (format #f \"~a\" (module-name m)))
     ((@ (ares reflection modules) all-modules)))")))

(defun arei--get-module-filename (module)
  (let ((value
         (arei--get-expression-value
          (format "((@ (ares reflection modules) module-filename)
(resolve-module '%s))"
                  module))))
    (unless (string= value "#f")
      (read value))))

(defun arei-goto-module (module)
  "Go to module source file if it is available."
  (interactive
   (list
    (completing-read "Module: " (arei--get-modules) nil t)))
  (let ((filename (arei--get-module-filename module)))
    (if filename
      (find-file filename)
      (message "This module doesn't have corresponding filename. (Or \
we couldn't figure it out)"))))

(defun arei-reload-module ()
  (interactive)
  (let* ((module (arei-current-module))
         (response
          (arei--sync-eval
           (format "\
(let ((m (resolve-module '%s #:ensure #f)))
  (if m
      (begin
        (module-clear! m)
        (reload-module m))
      'module-not-found))
" module)))
         (status (arei-nrepl-dict-get response "status"))
         (value (arei-nrepl-dict-get response "value")))
    (when (equal '("done") status)
      (if (string= "module-not-found" value)
          (message "%s: Module not found. Please make sure it's on \
load-path or create it manually." module)
        (message "%s: Module reloaded." module)))
    (when (member "interrupted" status)
      (error "%s: Module reloading takes too much time, check
if there are any top-level forms causing infinite recursions,
expensive computations or something like that.

If you are sure you module is right, increase
`arei-client-sync-timeout'.  See `arei-client-send-sync-request'
for more options how to do that temporary."
               module))))

(defvar-keymap arei-module-map
  "M-r" #'arei-reload-module
  "M-m" #'arei-goto-module)

(provide 'arei-module)
