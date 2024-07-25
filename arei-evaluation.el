;;; arei-evaluation.el --- Evaluation for Arei  -*- lexical-binding: t; -*-

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

;; Evaluation for Arei

;;; Code:

(require 'arei-client)
(require 'arei-syntax)
(require 'arei-nrepl)
(require 'arei-overlay)
(require 'eros)

(eval-when-compile (require 'map))
(eval-when-compile (require 'pcase))

(defun arei--send-stdin ()
  (arei-client-send-request
   (arei-nrepl-dict
    "op" "stdin"
    "stdin"
    (condition-case nil
        (concat (read-from-minibuffer "Stdin: " nil) "\n")
      (quit nil)))
   #'ignore
   (arei--user-evaluation-session-id)))

(defun arei--process-user-eval-response-callback
    (connection-buffer &optional expression-end)
  "Set up a handler for eval request responses."
  (let ((vals nil))
    (lambda (response)
      (pcase response
        ((map status value out err)
         ;; Jump to specific position only when there is something to print
         (when (or out err value)
           (goto-char (point-max)))

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
                       (ignore-errors (apply #'forward-sexp args))))
                    (truncate-lines nil))
               (unless (eros--make-result-overlay (or value "") ; response
                         :format fmt
                         :where (or expression-end (point))
                         :duration eros-eval-result-duration)
                 (message fmt value)))))

         (when (get-buffer-window)
           (set-window-point (get-buffer-window) (buffer-size))))))))

(defun arei--request-user-eval (code &optional bounds)
  (arei-overlay-blink-region bounds)
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
    (arei-client-send-request
     request
     (arei--process-user-eval-response-callback (current-buffer) end)
     (arei--user-evaluation-session-id))))


;;;
;;; APIs
;;;

(defun arei-interrupt-evaluation (&optional session-id)
  "Interrupt evaluation for a particular SESSION-ID, if no
SESSION-ID specified interrupt default user's evaluation session."
  (interactive)
  (arei-client-send-request
   (arei-nrepl-dict "op" "interrupt")
   #'ignore
   (or session-id (arei--user-evaluation-session-id))))

(defun arei-interrupt-tooling-evaluation ()
  "Interrupt evaluation in tooling session."
  (interactive)
  (arei-interrupt-evaluation (arei--tooling-session-id)))

(defun arei--sync-eval-timeout-callback (session-id)
  "Interrupt evaluation, when request timeouted."
  (lambda (final-request)
    (arei-client-send-sync-request
     (arei-nrepl-dict
      "op" "interrupt"
      "interrupt-id" (arei-nrepl-dict-get final-request "id"))
     session-id)))

(defun arei--sync-eval (exp &optional module session-id)
  "Try to syncronously evaluate EXP and if timeout reached, interrupt
evaluation.  You can dynamically bind `arei-client-sync-timeout'
to change evaluation timeout.

Example:
(let ((arei-client-sync-timeout 10))
  (arei--get-expression-value \"(begin (sleep 7) \\='hi)\"))

If MODULE is set evaluate in its context, otherwise use
`arei-current-module'.

If SESSION-ID is set use it, otherwise call
`arei--tooling-session-id' and use its value."
  (let ((request (arei-nrepl-dict
                  "op" "eval"
                  "code" exp)))
    (when-let* ((module (or module (arei-current-module))))
      (arei-nrepl-dict-put request "ns" module))
    (let ((session-id (or session-id (arei--tooling-session-id))))
      (arei-client-send-sync-request
       request
       session-id
       (arei--sync-eval-timeout-callback session-id)))))

(defun arei--get-expression-value (exp)
  (arei-nrepl-dict-get (arei--sync-eval exp) "value"))

(defun arei--eval (exp &optional callback module session-id)
  "Asncronously evaluate EXP.

If MODULE is set evaluate in its context, otherwise use
`arei-current-module'.

If SESSION-ID is set use it, otherwise call
`arei--tooling-session-id' and use its value.
"
  (let ((request (arei-nrepl-dict
                  "op" "eval"
                  "code" exp)))
    (when-let* ((module (or module (arei-current-module))))
      (arei-nrepl-dict-put request "ns" module))
    (let ((session-id (or session-id (arei--tooling-session-id))))
      (arei-client-send-request
       request
       (or callback (lambda (response) (message "response: %s" response)))
       session-id))))


;;;
;;; Wrappers for user-eval
;;;

(defun arei-evaluate-region (start end)
  (interactive "r")
  (arei--request-user-eval nil (cons start end)))

(defun arei-evaluate-sexp (exp)
  "Evaluate EXP.  When called interactively, read an expression and
evaluate it.  It's similiar to Emacs' `eval-expression' by spirit."
  (interactive
   (list (read-from-minibuffer "Expression: " nil nil nil 'arei-expression)))
  (arei--request-user-eval exp))

(defun arei-evaluate-last-sexp ()
  (interactive)
  (arei--request-user-eval nil (arei-syntax-last-sexp-bounds)))

(defun arei-evaluate-buffer ()
  (interactive)
  (arei--request-user-eval nil (arei-syntax-buffer-bounds)))

(defun arei-evaluate-enclosing-outer-form ()
  "Evaluate top-level form AKA defun.

In the future it may start accepting universal argument to narrow
down to non-top level forms."
  (interactive)
  (arei--request-user-eval nil (arei-syntax-current-top-level-form)))

(defun arei-evaluate-enclosing-inner-form ()
  "Evaluate nearest enclosing form, basically a list under point.

In the future it may start accepting universal argument to widen
up to next enclosing forms."
  (interactive)
  (arei--request-user-eval nil (arei-syntax-list-at-point)))

(defvar-keymap arei-evaluation-keymap
  "C-e" #'arei-evaluate-last-sexp
  "C-b" #'arei-evaluate-buffer
  "C-r" #'arei-evaluate-region
  "C-t" #'arei-evaluate-enclosing-outer-form
  ":" #'arei-evaluate-sexp
  "C-i" #'arei-interrupt-evaluation)

(provide 'arei-evaluation)
;;; arei-evaluation.el ends here
