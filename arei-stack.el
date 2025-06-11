;;; arei-stack.el --- Displaying the scheme stack    -*- lexical-binding: t; -*-

;; Copyright © 2025 Libre en Communs <contact@a-lec.org>
;; Copyright © 2025 Noé Lopez <noelopez@free.fr>

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

;;; Commentary:

;; For debugging, we need to have a buffer to display backtraces and
;; stack frames.

;;; Code:

;; FIXME: for already opened files, we can set a marker to keep the
;; right position even if the code is modified
(defun arei--goto-source (source)
  (let* ((file (arei-nrepl-dict-get source "file"))
         (line (arei-nrepl-dict-get source "line"))
         (column (arei-nrepl-dict-get source "column"))
         (buffer (find-file-noselect file)))
    (pop-to-buffer buffer #'display-buffer-reuse-window)
    (goto-char (point-min))
    (forward-line line)
    (forward-char column)))

(defun arei--insert-stack (stack)
  (insert "Stack:\n")
  (dolist (frame stack)
    (let ((.source (arei-nrepl-dict-get frame "source"))
          (.procedure-name (arei-nrepl-dict-get frame "procedure-name"))
          (.arguments (arei-nrepl-dict-get frame "arguments")))
      (insert "  (")
      (if .source
          (insert-button .procedure-name
                         'action (lambda (_)
                                   (arei--goto-source .source)))
        (insert .procedure-name))
      (mapcar (lambda (arg) (insert " " arg)) .arguments)
      (insert ")\n"))))

(provide 'arei-stack)
;;; arei-stack.el ends here
