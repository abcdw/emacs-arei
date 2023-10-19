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
;;; arei-connection-mode
;;;

(defvar arei-connection-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'sesman-map)
    (easy-menu-define cider-repl-mode-menu map
      "Menu for Arei's CONNECTION mode"
      `("CONNECTION"))
    map))

(define-derived-mode arei-connection-mode fundamental-mode "kinda REPL"
  "Major mode for Arei connection.

\\{arei-connection-mode-map}"
  ;; :keymap arei-mode-map
  (setq-local sesman-system 'Arei))


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
;;; Sessions
;;;

;; (sesman-current-sessions 'Arei)

;; (defun arei-sessions ()
;;   "Return a list of all active Arei sessions."
;;   (sesman-sessions 'Arei))

(cl-defmethod sesman-project ((_system (eql Arei)))
  "Find project directory."
  (project-root (project-current)))

(cl-defmethod sesman-start-session ((_system (eql Arei)))
  "Start a connection."
  (call-interactively #'arei))

(cl-defmethod sesman-quit-session ((_system (eql Arei)) session)
  "Quit a CIDER SESSION."
  (let ((kill-buffer-query-functions nil))
    (kill-buffer (cadr session))))


;;;
;;; Rest
;;;

;; (require 'monroe)
(defun arei-net-filter (process string)
  "Called when the new message is received. Process will redirect
all received output to this function; it will decode it and put in
monroe-repl-buffer."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string)
    ;; Stolen from Cider. Assure we have end of the message so
    ;; decoding can work; to make sure we are at the real end (session
    ;; id can contain 'e' character), we call 'accept-process-output'
    ;; once more.
    ;;
    ;; This 'ignore-errors' is a hard hack here since
    ;; 'accept-process-output' will call filter which will be this
    ;; function causing Emacs to hit max stack size limit.
    (ignore-errors
        (when (eq ?e (aref string (- (length string) 1)))
          (unless (accept-process-output process 0.01)
            (while (> (buffer-size) 1)
              (mapc #'monroe-dispatch (monroe-net-decode))))))))

(defun arei--sentinel (process message)
  "Called when connection is changed; in out case dropped."
  (message "nREPL connection closed: %s" message)
  (kill-buffer (process-buffer process)))

(defun arei--server-reply (process content)
   "Gets invoked whenever the server sends data to the client."
   (with-current-buffer (process-buffer process)
     (insert (format "%s" (rail-bencode-decode content)))
     (insert "\n")))

(defun arei-connect ()
  "Connect to remote endpoint using provided hostname and port."
  (let* ((host "localhost")
         (port "7888")
         (host-and-port (concat host ":" port))
         (project (project-current))
         (buffer-name (concat "*arei-connection: " host-and-port "*"))
         (session-name (concat (project-name project) ":" host-and-port)))
    (message "Connecting to nREPL host on '%s:%s'..." host port)

    (let* ((process (open-network-stream
                     (concat "nrepl-connection-" host-and-port)
                     buffer-name host port))
           (buffer (process-buffer process)))
      (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
      (set-process-filter process 'arei--server-reply)

      (with-current-buffer buffer
        (arei-connection-mode)
        (setq-local default-directory (project-root (project-current))))
      (set-process-sentinel process 'arei--sentinel)
      (sesman-add-object 'Arei session-name buffer 'allow-new)
      (process-send-string process "d2:op5:clonee")

      ;; (monroe-send-hello (monroe-new-session-handler buffer))
      (display-buffer buffer)
      buffer)))

;;;###autoload
(defun arei ()
  "Connect to nrepl server."
  (interactive)
  (message "I'll try to connect soon")
  (arei-connect))

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
