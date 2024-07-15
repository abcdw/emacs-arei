;;; arei-guile-evaluation.el --- Guile Evaluation for Arei  -*- lexical-binding: t; -*-

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

;; Guile Evaluation for Arei

;;; Code:

(require 'arei-client)
(require 'arei-syntax)
(require 'arei-nrepl)

(eval-when-compile (require 'map))
(eval-when-compile (require 'pcase))

(defun arei--send-stdin ()
  (arei-send-request
   (arei-nrepl-dict
    "op" "stdin"
    "stdin"
    (condition-case nil
        (concat (read-from-minibuffer "Stdin: " nil) "\n")
      (quit nil)))
   (arei-connection-buffer)
   #'ignore
   (arei-client--get-session-id "evaluation")))


(defun arei--process-user-eval-response-callback
    (connection-buffer &optional expression-end)
  "Set up a handler for eval request responses."
  (let ((vals nil))
    (lambda (response)
      (pcase response
        ((map status value out err)
         (goto-char (point-max))

         (when (member "need-input" status)
           (arei--send-stdin))
         (when out
           (insert out))
         (when err
           (insert (propertize err 'face
                               '((t (:inherit font-lock-warning-face))))))
         (when value
           (unless (= 0 (current-column))
             (insert "\n"))
           (insert (propertize value 'face
                               '((t (:inherit font-lock-string-face)))))
           (insert "\n"))

         (when (and (member "multiple-values" status)
                    (not (member "done" status)))
           (push value vals))
         (when (member "done" status)
           (with-current-buffer connection-buffer
             (let* ((value (or (and (member "multiple-values" status)
                                    (if vals
                                        (mapconcat
                                         (lambda (v) (concat " => " v))
                                         (reverse vals)
                                         "\n")
                                      " => "))
                               (and value (concat " => " value))))
                    (fmt (if value "%s" " ;; interrupted"))
                    (forward-sexp-function
                     (lambda (&rest args)
                       ;; see https://github.com/xiongtx/eros/issues/10
                       (ignore-errors (apply #'forward-sexp args)))))
               (unless (eros--make-result-overlay (or value "") ; response
                         :format fmt
                         :where (or expression-end (point))
                         :duration eros-eval-result-duration)
                 (message fmt value))))
           ;; NOTE: stop spinner if it's the last request (we can have
           ;; multiple evals queued)
           (when (= 1 (hash-table-count arei-client--pending-requests))
             (arei-spinner-stop)))

         (when (get-buffer-window)
           (set-window-point (get-buffer-window) (buffer-size))))))))

(defun arei--get-evaluation-value-callback (_connection-buffer)
  "Set up a handler for eval request responses.  Ignores
stdout/stderr, saves value to `arei-eval-value' buffer-local
variable."
  (lambda (response)
    (pcase response
      ((map status value)
       (when (member "need-input" status)
         (arei--send-stdin))
       (setq-local arei-eval-value value)))))

(defun arei--request-user-eval (code &optional bounds)
  (pcase-let* ((`(,start . ,end) bounds)
               (code (or code
                         (buffer-substring-no-properties start end)))
               (request (arei-nrepl-dict
                         "op" "eval"
                         "code" code
                         "file" (buffer-file-name))))
    (when-let* ((module (arei-current-module)))
      (arei-nrepl-dict-put request "ns" module))
    (when-let* ((line (and start (1- (line-number-at-pos start)))))
      (arei-nrepl-dict-put request "line" line))
    (when-let* ((column (and start (save-excursion
                                     (goto-char start)
                                     (current-column)))))
      (arei-nrepl-dict-put request "column" column))
    (arei-send-request
     request
     (arei-connection-buffer)
     (arei--process-user-eval-response-callback (current-buffer) end)
     (arei-client--get-session-id "evaluation"))
    (ignore-errors (arei-spinner-start))))

(defun arei--request-eval (code &optional connection)
  (let ((request (arei-nrepl-dict
                  "op" "eval"
                  "code" code))
        (module (arei-current-module)))
    (when module
      (arei-nrepl-dict-put request "ns" module))
    (arei-send-request
     request
     (or connection (arei-connection-buffer))
     (arei--get-evaluation-value-callback (current-buffer))
     t)))

(defun arei-interrupt-evaluation (&optional session-id)
  "Interrupt evaluation for a particular SESSION-ID, if no
SESSION-ID specified interrupt default user's evaluation session."
  (interactive)
  (arei-send-request
   (arei-nrepl-dict "op" "interrupt")
   (arei-connection-buffer)
   #'ignore
   (or session-id (arei-client--get-session-id "evaluation"))))

(defun arei-evaluate-region (start end)
  (interactive "r")
  (arei--request-user-eval nil (cons start end)))

(defun arei-evaluate-sexp (exp)
  "Evaluate EXP.  When called interactively, read an expression and
evaluate it.  It's similiar to Emacs' `eval-expression' by spirit."
  (interactive
   (list (read-from-minibuffer "Expression: " nil nil nil 'arei-expression)))
  (arei--request-user-eval exp))

(defun arei--get-expression-value (exp &optional connection)
  (let ((request (arei-nrepl-dict
                  "op" "eval"
                  "code" exp)))
    (when-let* ((module (arei-current-module)))
      (arei-nrepl-dict-put request "ns" module))
    (thread-first
      request
      (arei-send-sync-request connection t)
      (arei-nrepl-dict-get "value"))))

(defun arei-evaluate-last-sexp ()
  (interactive)
  (arei--request-user-eval nil (arei-syntax-last-sexp-bounds)))

(defun arei-evaluate-buffer ()
  (interactive)
  (arei--request-user-eval nil (arei-syntax-buffer-bounds)))

(defun arei-evaluate-defun ()
  (interactive)
  (arei--request-user-eval nil (arei-syntax-current-top-level-form)))

(provide 'arei-guile-evaluation)
;;; arei-guile-evaluation.el ends here
