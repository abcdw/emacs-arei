;;; arei.el --- Asynchronous Reliable Extensible IDE -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright © 2023 Andrew Tropin <andrew@trop.in>

;; Author: Andrew Tropin <andrew@trop.in>
;;
;; URL: https://trop.in/rde
;; Package-Requires: ((emacs "29") (spinner "1.7") (sesman "0.3.2"))
;; Keywords: languages, guile, scheme, nrepl

;; This program is free software: you can redistribute it and/or modify
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

;; Interface for fast navigation between hunks and easier access to various
;; git commands.

;;; Code:

(require 'sesman)
(require 'eros)

;;;
;;; arei-mode
;;;

(defun arei--modeline-info ()
  "Return info for the arei mode modeline.
Info contains the connection type, project name and host:port endpoint."
  "not connected")

(defvar arei-mode-line '(:eval (format " arei[%s]" (arei--modeline-info))) "\
Mode line lighter for are mode.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for details
about mode line templates.

Customize this variable to change how arei mode displays its status in the
mode line.  The default value displays the current connection.  Set this
variable to nil to disable the mode line entirely.")

(defconst arei-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'arei-eval-last-sexp)
    (define-key map (kbd "C-c C-e") #'arei-eval-last-sexp)
    (define-key map (kbd "C-c C-b") #'arei-interrupt)
    (define-key map (kbd "C-c M-r") #'arei-restart)
    map))

;;;###autoload
(define-minor-mode arei-mode
  "Minor mode for REPL interaction from a buffer.

\\{arei-mode-map}"
  :init-value nil
  :lighter arei-mode-line
  :keymap arei-mode-map
  (if arei-mode
      (progn
        (setq-local sesman-system 'Arei)
        ;; (arei-eldoc-setup)
        ;; (add-hook 'completion-at-point-functions #'arei-complete-at-point nil t)
        ;; (add-hook 'xref-backend-functions
        ;;           #'arei--xref-backend arei-xref-fn-depth 'local)
        ;; (setq next-error-function #'arei-jump-to-compilation-error)
        )
    ;; Mode cleanup
    ;; (mapc #'kill-local-variable '(next-error-function))
    ;; (remove-hook 'completion-at-point-functions #'arei-complete-at-point t)
    ;; (remove-hook 'xref-backend-functions #'arei--xref-backend 'local)
    ))

;;;
;;; Rest
;;;

;;;###autoload
(defun arei ()
  "Connect to nrepl server."
  (interactive)
  (message "I'll try to connect soon"))

;;;###autoload
(with-eval-after-load 'scheme
  (define-key scheme-mode-map (kbd "C-c C-a") #'arei)
  (define-key scheme-mode-map (kbd "C-c C-s") 'sesman-map)
  (require 'sesman)
  (sesman-install-menu scheme-mode-map)
  (add-hook 'scheme-mode-hook (lambda () (setq-local sesman-system 'Arei))))

;; (defun arei-eros--eval-last-sexp (result)
;;   "Show RESULT in EROS overlay at point."
;;   (eros--make-result-overlay
;;    result
;;    :format " %s"
;;    :where (point)
;;    :duration eros-eval-result-duration)
;;   result)

;; (define-minor-mode arei-eros-mode
;;   "Display Scheme evaluation results overlays."
;;   :global t
;;   :group 'geiser
;;   (if geiser-eros-mode
;;       (progn
;;         (advice-add 'monroe-eval-expression-at-point
;;                     :filter-return #'arei-eros--eval-last-sexp))
;;     (advice-remove 'monroe-eval-expression-at-point
;;                    #'arei-eros--eval-last-sexp)))


;; (message "hi")


;; TODO: Scratch buffer
