;;; arei-spinner.el --- Evaluation spinner for arei  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Nikita Domnitskii

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

;; Evaluation spinner for arei

;;; Code:

(require 'ring)

(defvar arei-spinner--symbol nil)
(defvar arei-spinner--timer (timer-create))
(defvar arei-spinner--ring
  (ring-convert-sequence-to-ring
   ;; TODO: [Nikita Domnitskii, 2024-05-06] should be a custom
   '("⣷" "⣯" "⣟" "⡿" "⢿" "⣻" "⣽" "⣾")))

(defun arei-spinner--tick ()
  (setq arei-spinner--symbol
        (ring-next arei-spinner--ring arei-spinner--symbol))
  (force-mode-line-update t))

(defun arei-spinner-start ()
  (let* ((repeat 0.1)
         (time (timer-next-integral-multiple-of-time (current-time) repeat)))
    (setq arei-spinner--symbol (ring-ref arei-spinner--ring 0))
    (timer-set-time arei-spinner--timer time repeat)
    (timer-set-function arei-spinner--timer #'arei-spinner--tick)
    (timer-activate arei-spinner--timer)))

(defun arei-spinner-stop ()
  (cancel-timer-internal arei-spinner--timer)
  (setq arei-spinner--symbol nil))

(defun arei-spinner-modeline ()
  (and arei-spinner--symbol (list arei-spinner--symbol)))

(provide 'arei-spinner)
;;; arei-spinner.el ends here
