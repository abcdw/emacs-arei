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

(defun arei--insert-stack (stack)
  (insert "Backtrace:\n")
  (dolist (frame stack)
    (let ((.source (arei-nrepl-dict-get frame "source"))
          (.procedure-name (arei-nrepl-dict-get frame "procedure-name"))
          (.arguments (arei-nrepl-dict-get frame "arguments")))
      (insert "(")
      (if .source
          (insert-button .procedure-name
                         'follow-link t
                         'action (lambda (_)
                                   (find-file-other-window (arei-nrepl-dict-get .source "file") nil)
                                   (goto-char (point-min))
                                   (forward-line (arei-nrepl-dict-get .source "line"))
                                   (forward-char (arei-nrepl-dict-get .source "column"))))
        (insert .procedure-name))
      (mapcar (lambda (arg) (insert " " arg)) .arguments)
      (insert ")\n"))))

(provide 'arei-stack)
;;; arei-stack.el ends here
