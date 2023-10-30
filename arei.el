;;; arei.el --- Asynchronous Reliable Extensible IDE -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright © 2023 Andrew Tropin <andrew@trop.in>

;; Author: Andrew Tropin <andrew@trop.in>
;;
;; URL: https://trop.in/rde
;; Package-Requires: ((emacs "29") (eros "0.1.0") (sesman "0.3.2") (queue "0.2"))
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

;; An Interactive Development Environment for Guile

;;; Code:

(require 'sesman)
(require 'arei-nrepl)
(require 'eros)

(defgroup arei nil
  "Asynchronous Reliable Extensible IDE."
  :prefix "arei-"
  :group 'applications)

(defcustom arei-mode-auto-p t
  "Whether `arei-mode' should be active by default in all scheme buffers."
  :type 'boolean)

(defvar-local arei--request-counter 0
  "Serial number for message, used for association between request
and responses.")

(defvar-local arei--nrepl-pending-requests nil
  "A hash-table containing callbacks for pending requests.")


;;;
;;; Sessions
;;;

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
  "Quit an Arei SESSION."
  (let ((kill-buffer-query-functions nil))
    (kill-buffer (cadr session))))


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

\\{arei-connection-mode-map}
"

  ;; :keymap arei-mode-map
  ;; A smooth scrolling instead of jumping half a screen:
  (setq-local scroll-conservatively 101)
  (setq-local sesman-system 'Arei))


;;;
;;; Connection
;;;

(defun arei--sentinel (process message)
  "Called when connection is changed; in out case dropped."
  (message "nREPL connection closed: %s" message)
  (kill-buffer (process-buffer process)))

;; TODO: [Andrew Tropin, 2023-10-19] Handle incomplete incomming string.
(defun arei--connection-filter (process string)
  "Decode message(s) from PROCESS contained in STRING and dispatch them."
  (let ((string-q (process-get process :string-q)))
    (queue-enqueue string-q string)
    ;; Start decoding only if the last letter is 'e'
    (when (eq ?e (aref string (1- (length string))))
      (let ((response-q (process-get process :response-q)))
        (nrepl-bdecode string-q response-q)
        (while (queue-head response-q)
          (with-current-buffer (process-buffer process)
            (let ((response (queue-dequeue response-q)))
              ;; (message "response: %s\n" response)
              ;; (with-demoted-errors
              ;;     "Error in one of the `nrepl-response-handler-functions': %s"
              ;;   (run-hook-with-args 'nrepl-response-handler-functions response))
              (arei--dispatch-response response))))))))

(defun arei--dispatch-response (response)
  "Find associated callback for a message by id."
  (nrepl-dbind-response response (id)
    (let ((callback (gethash id arei--nrepl-pending-requests)))
      (when callback
        (funcall callback response)))))

(defun arei-connection-buffer ()
  "Returns a connection buffer associated with the current session."
  (cadr (sesman-current-session 'Arei)))

(defun arei-connection ()
  "Returns a process associated with the current session connection."
  (get-buffer-process (arei-connection-buffer)))

(defun arei-send-request (request callback)
  "Send REQUEST and assign CALLBACK.
The CALLBACK function will be called when reply is received."
  (with-current-buffer (arei-connection-buffer)
    (let* ((id (number-to-string (cl-incf arei--request-counter))))
      (nrepl-dict-put request "id" id)
      (puthash id callback arei--nrepl-pending-requests)
      (process-send-string nil (nrepl-bencode request)))))

(defun arei--current-nrepl-session ()
  (with-current-buffer (arei-connection-buffer)
    arei--nrepl-session))

(defun eros--remove-result-overlay-real ()
  "Remove result overlay from current buffer.

This function also removes itself from `pre-command-hook'."
  (remove-hook 'post-command-hook #'eros--remove-result-overlay-real 'local)
  (remove-overlays nil nil 'category 'result))

(defun eros--remove-result-overlay ()
  "Setup a callback to remove result overlay from current buffer."
  (remove-hook 'pre-command-hook #'eros--remove-result-overlay 'local)
  (add-hook 'post-command-hook #'eros--remove-result-overlay-real nil 'local))

(defun arei--send-stdin ()
  (arei--send-request-with-session
   (nrepl-dict
    "op" "stdin"
    "stdin"
    (condition-case nil
        (concat (read-from-minibuffer "Stdin: " nil) "\n")
      (quit nil)))
   (lambda (response) 'hi)))

(defun arei--process-eval-response-callback (source-buffer)
  (lambda (response)
    (nrepl-dbind-response response (id status value out err op)
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
      (when (member "done" status)
        (with-current-buffer source-buffer
          (eros--make-result-overlay
              ;; response
              (or value "")
            :format (if value " => %s" " ;; interrupted")
            :where (point)
            :duration eros-eval-result-duration))
        (remhash id arei--nrepl-pending-requests))

      (when (get-buffer-window)
        (set-window-point (get-buffer-window) (buffer-size))))))

(defun arei--send-request-with-session (request callback)
  (nrepl-dict-put request "session" (arei--current-nrepl-session))
  (arei-send-request request callback))

(defun arei--request-eval (code)
  (arei--send-request-with-session
   (nrepl-dict
    "op" "eval"
    "code" code)
   (arei--process-eval-response-callback (current-buffer))))

(defun arei-interrupt-evaluation ()
  (interactive)
  (arei--send-request-with-session
   (nrepl-dict "op" "interrupt")
   (lambda (response) 'hi)))

(defun arei-evaluate-region (start end)
  (interactive "r")
  (arei--request-eval (buffer-substring-no-properties start end)))

(defun arei-evaluate-last-sexp ()
  (interactive)
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (arei-evaluate-region (point) end))))

(defun arei--new-session-handler ()
  "Returns callback that is called when new connection is established."
  (lambda (response)
    (nrepl-dbind-response
        response (id new-session)
      (when new-session
        (insert
         (propertize
          ";;; Connected\n"
          'face
          '((t (:inherit font-lock-comment-face)))))
        (message "Connected to nREPL server.")
        (setq-local arei--nrepl-session new-session)
        (remhash id arei--nrepl-pending-requests)))))

(defun arei--print-pending-requests ()
  (interactive)
  (with-current-buffer (arei-connection-buffer)
      (maphash (lambda (key value)
                 (message "Key: %s, Value: %s" key value))
               arei--nrepl-pending-requests)))

(defun arei-switch-to-connection-buffer ()
  (interactive)
  (pop-to-buffer (arei-connection-buffer)))

(defun arei--initialize-session ()
  ;; TODO: [Andrew Tropin, 2023-10-19] Probably it's better to make it
  ;; syncronous to prevent eval requests before nrepl session created
  (arei-send-request (nrepl-dict "op" "clone") (arei--new-session-handler)))

(defun arei-connect ()
  "Connect to remote endpoint using provided hostname and port."
  (let* ((host "localhost")
         (port "7888")
         (host-and-port (concat host ":" port))
         (project (project-current))
         (buffer-name (concat "*arei-connection: " host-and-port "*"))
         (sesman-session-name (concat (project-name project) ":" host-and-port)))

    (let* ((process (open-network-stream
                     (concat "nrepl-connection-" host-and-port)
                     buffer-name host port))
           (buffer (process-buffer process)))
      (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
      (set-process-filter process 'arei--connection-filter)
      (set-process-sentinel process 'arei--sentinel)
      (process-put process :string-q (queue-create))
      (process-put process :response-q (nrepl-response-queue))

      (with-current-buffer buffer
        (arei-connection-mode)
        (setq arei--request-counter 0)
        (setq arei--nrepl-pending-requests (make-hash-table :test 'equal))
        (setq-local arei--nrepl-session nil)
        (setq default-directory (project-root (project-current)))
        (insert
         (propertize
          (format ";;; Connecting to nREPL host on '%s:%s'...\n" host port)
          'face
          '((t (:inherit font-lock-comment-face))))))

      (sesman-add-object 'Arei sesman-session-name buffer 'allow-new)
      (arei--initialize-session)
      (display-buffer buffer)
      buffer)))


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
    (define-key map (kbd "C-x C-e") #'arei-evaluate-last-sexp)
    (define-key map (kbd "C-c C-e") #'arei-evaluate-last-sexp)
    (define-key map (kbd "C-c C-b") #'arei-interrupt-evaluation)
    (define-key map (kbd "C-c C-z") #'arei-switch-to-connection-buffer)

    ;; (define-key map (kbd "C-c C-b") #'arei-interrupt)
    ;; (define-key map (kbd "C-c M-r") #'arei-restart)
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
        (setq sesman-system 'Arei)
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

(defun arei-mode--maybe-activate ()
  "Activates `arei-mode' if `arei-mode-auto-p' is t."
  (when arei-mode-auto-p
    (arei-mode)))

;;;###autoload
(defun arei ()
  "Connect to nrepl server."
  (interactive)
  (arei-connect))

;;;###autoload
(with-eval-after-load 'scheme
  (define-key scheme-mode-map (kbd "C-c C-a") #'arei)
  (define-key scheme-mode-map (kbd "C-c C-s") 'sesman-map)
  (require 'sesman)
  (sesman-install-menu scheme-mode-map)
  (add-hook 'scheme-mode-hook 'arei-mode--maybe-activate))

;; TODO: Scratch buffer
