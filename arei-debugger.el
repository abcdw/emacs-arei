;;; arei-debugger.el --- Debugger functionality for Arei    -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2025 Libre en Communs <contact@a-lec.org>
;; SPDX-FileCopyrightText: 2025 Noé Lopez <noelopez@free.fr>
;; SPDX-FileCopyrightText: 2025 Andrew Tropin <andrew@trop.in>

;; Author: Noé Lopez <noelopez@free.fr>

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

;;; Code:

(require 'arei-stack)

(defvar-keymap arei-debugger-mode-map
  :doc "Keymap for debugging actions."
  "n" #'forward-button
  "<tab>" #'forward-button
  "M-n" #'arei-debugger-next-frame
  "p" #'backward-button
  "<backtab>" #'backward-button
  "M-p" #'arei-debugger-previous-frame)

(define-derived-mode arei-debugger-mode
  special-mode "Arei Debugger"
  "Major mode for debugging an Arei evaluation."
  '())

(defun arei-debugger-next-frame (n)
  (interactive "p")
  (forward-button n)
  (save-excursion (push-button)))

(defun arei-debugger-previous-frame (n)
  (interactive "p")
  (backward-button n)
  (save-excursion (push-button)))

(defun arei--debugger-buffer ()
  ;; FIXME: integrate with sesman so that buffer is closed with connection?
  (let* ((name (car (sesman-current-session 'Arei)))
         (buffer (get-buffer-create (concat "*arei-debugger: " name "*"))))
    buffer))

(defun arei-show-debugger (err stack)
  (let ((buffer (arei--debugger-buffer)))
    (with-current-buffer buffer
      (arei-debugger-mode)
      (toggle-truncate-lines 1)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (arei--insert-stack stack)
        (insert "\n")

        (when err
          (insert "Exception:\n")
          (insert
           (propertize err 'face '((t (:inherit font-lock-warning-face)))))
          (insert "\n"))

        (goto-char (point-min))))
    (when (fboundp arei-debugger-buffer-display-function)
      (funcall arei-debugger-buffer-display-function buffer))))

(provide 'arei-debugger)
;;; arei-debugger.el ends here
