;;; arei.el --- Asynchronous Reliable Extensible IDE -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright © 2023, 2024 Andrew Tropin
;; Copyright © 2024 Nikita Domnitskii

;; Author: Andrew Tropin <andrew@trop.in>
;;         Nikita Domnitskii <nikita@domnitskii.me>
;;
;; Version: 0.9.5
;; Homepage: https://trop.in/rde
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
(require 'arei-client)
(require 'arei-nrepl)
(require 'arei-eldoc)
(require 'arei-xref)
(require 'arei-completion)
(require 'arei-spinner)
(require 'arei-evaluation)
(require 'arei-module)
(require 'arei-macroexpansion)
(require 'scheme)
(require 'sesman)
(require 'eros)
(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'pcase))
(eval-when-compile (require 'map))

(defgroup arei nil
  "Asynchronous Reliable Extensible IDE."
  :prefix "arei-"
  :group 'applications)

(defcustom arei-mode-auto t
  "Whether `arei-mode' should be active by default in all scheme buffers."
  :type 'boolean)

(defcustom arei-connection-buffer-display-function 'display-buffer
  "Controls how to display the connection buffer on connect.

When set to nil the buffer will only be created, and not
displayed.  When it set to function the buffer will be displayed
using this function.  `display-buffer' and `pop-to-buffer' are
most obvious candidates here."
  :type '(choice (function :tag "Function")
                 (const :tag "None" nil)))

(defun arei--get-command-keybindings (command)
  "Return key bindings for COMMAND as a comma-separated string."
  (let ((keys (mapcar 'key-description (where-is-internal command nil nil t))))
    (if keys
        (mapconcat 'identity keys ", ")
      "M-x ... RET")))

(defcustom arei-get-greeting-message
  (lambda ()
    (apply
     'format
     "Commands for session and connection management:
- `sesman-start' (%s) to connect to nrepl server.
- `universal-argument' (%s) to select a connection endpoint (host and port).
- `sesman-quit' (%s) to close the connection.

Development related and other commands:
- `arei-evaluate-last-sexp' (%s) to evaluate expression before point.
- `arei-evaluate-buffer' (%s) to evaluate expression before point.
- `arei-evaluate-sexp' (%s) to interactively evaluate the expression."
     (mapcar 'arei--get-command-keybindings
             `(sesman-start
               universal-argument
               sesman-quit
               arei-evaluate-last-sexp
               arei-evaluate-buffer
               arei-evaluate-sexp))))
  "A function returning a message shown on connection creation"
  :type 'function)


;;;
;;; Sessions
;;;

;; (defun arei-sessions ()
;;   "Return a list of all active Arei sessions."
;;   (sesman-sessions 'Arei))

(cl-defmethod sesman-project ((_system (eql Arei)))
  "Find project directory."
  (when (project-current)
    (project-root (project-current))))

(cl-defmethod sesman-start-session ((_system (eql Arei)))
  "Start a connection."
  (call-interactively #'arei--start))

(cl-defmethod sesman-friendly-session-p ((_system (eql Arei)) session)
  (let* ((conn (cadr session))
         (file (buffer-file-name))
         (load-path (thread-first
                      (arei-nrepl-dict "op" "ares.guile.utils/load-path")
                      (arei-send-sync-request conn t)
                      (arei-nrepl-dict-get "load-path"))))
    (seq-find (lambda (path) (string-prefix-p path file)) load-path)))

(cl-defmethod sesman-quit-session ((_system (eql Arei)) session)
  "Quit an Arei SESSION."
  (let ((kill-buffer-query-functions nil))
    (arei-spinner-stop)
    (kill-buffer (cadr session))))


;;;
;;; arei-connection-mode
;;;

(defvar-keymap arei-connection-mode-map
  "C-c C-s" #'sesman-map)

(easy-menu-define arei-connection-mode-menu arei-connection-mode-map
  "Menu for Arei's CONNECTION mode"
  `("CONNECTION"))

(define-derived-mode arei-connection-mode fundamental-mode "kinda REPL"
  "Major mode for Arei connection.

\\{arei-connection-mode-map}
"

  ;; :keymap arei-mode-map
  ;; A smooth scrolling instead of jumping half a screen:
  (setq-local scroll-conservatively 101)
  (setq-local sesman-system 'Arei))


;;;
;;; Overlay
;;;

(defun eros--remove-result-overlay-real ()
  "Remove result overlay from current buffer.

This function also removes itself from `pre-command-hook'."
  (remove-hook 'post-command-hook #'eros--remove-result-overlay-real 'local)
  (remove-overlays nil nil 'category 'result))

(defun eros--remove-result-overlay ()
  "Setup a callback to remove result overlay from current buffer."
  (remove-hook 'pre-command-hook #'eros--remove-result-overlay 'local)
  (add-hook 'post-command-hook #'eros--remove-result-overlay-real nil 'local))


;;;
;;; Connection
;;;

(defun arei--sentinel (process message)
  "Called when connection is changed; in out case dropped."
  (with-current-buffer (process-buffer process)
    (sesman-quit))
  (message "nREPL connection closed: %s" message)
  ;; NOTE: sesman-post-command-hook is not run when connection buffer is
  ;; killed, so we run it here just in case
  (run-hooks 'sesman-post-command-hook))

;; TODO: [Andrew Tropin, 2023-10-19] Handle incomplete incomming string.
(defun arei--connection-filter (process string)
  "Decode message(s) from PROCESS contained in STRING and dispatch them."
  (let ((string-q (process-get process :string-q)))
    (queue-enqueue string-q string)
    ;; Start decoding only if the last letter is 'e'
    (when (eq ?e (aref string (1- (length string))))
      (let ((response-q (process-get process :response-q)))
        (arei-nrepl-bdecode string-q response-q)
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
  (pcase response
    ((map id status)
     (when-let* ((callback (gethash id arei-client--pending-requests)))
       (when callback
         (funcall callback response)
         (when (member "done" status)
           (remhash id arei-client--pending-requests)))))))

;; TODO: [Andrew Tropin, 2023-11-20] Add association between session
;; and output buffer for it.  It's needed to support multiple nrepl
;; sessions that can use separate buffers for stdin/stdout instead of
;; using primary connection buffer.  Also, adding
;; `arei-set-default-nrepl-session' may help for eval and switch
;; operations.
(defun arei--new-session-handler (session-name &optional callback)
  "Returns callback that is called when new session is created."
  (lambda (response)
    (pcase response
      ((map new-session)
       (when new-session
         (insert
          (propertize
           (format ";;; Session created: %s\n" session-name)
           'face
           '((t (:inherit font-lock-comment-face)))))
         (message "Connected to nREPL server.")
         (when callback (funcall callback))
         (setq-local arei--nrepl-session new-session)
         (puthash session-name new-session arei-client--nrepl-sessions))))))

(defun arei--create-nrepl-session (connection session-name &optional callback)
  "Setups an nrepl session and register it in `arei--nrepl-sessions'."
  (puthash session-name nil arei-client--nrepl-sessions)
  (arei-send-request
   (arei-nrepl-dict "op" "clone")
   connection
   (arei--new-session-handler session-name callback)))

(defun arei--print-pending-requests ()
  (interactive)
  (with-current-buffer (arei-connection-buffer)
      (maphash (lambda (key value)
                 (message "Key: %s, Value: %s" key value))
               arei-client--pending-requests)))

;; MAYBE: Rename to switch-to-nrepl-session-buffer to make sure C-c
;; C-z jumps to buffer with needed output.
(defun arei-switch-to-connection-buffer ()
  (interactive)
  (if (arei-connection-buffer)
      (pop-to-buffer (arei-connection-buffer))
    (message "No connection associated with the buffer yet.")))

(defun arei--comment-string (str)
  "Add comment-prefix to "
  (let ((lines (split-string str "\n"))
        (comment-prefix ";;")
        (commented-lines '()))
    (dolist (line lines)
      (push (concat comment-prefix " " line) commented-lines))
    (string-join (reverse commented-lines) "\n")))

(defun arei--insert-greeting-message (initial-buffer)
  "Return a closure with captured INITIAL-BUFFER, which prints
greeting message."
  (lambda ()
    (insert
     (propertize
      (arei--comment-string
       (with-current-buffer initial-buffer
         (funcall arei-get-greeting-message)))
      'face
      '((t (:inherit font-lock-comment-face)))))
    (insert "\n")))

(defun arei--initialize-sessions (connection initial-buffer)
  "Initialize a session, use INITIAL-BUFFER to generate a correct
keybindings info in greeting message."
  (arei--create-nrepl-session
   connection
   "evaluation"
   (lambda ()))
  (arei--create-nrepl-session
   connection
   "tooling"
   (arei--insert-greeting-message initial-buffer)))

(defun arei--create-params-plist (arg)
  "Create initial PARAMS plist based on ARG vlaue."
  (cond ((equal arg '(4)) (list :select-endpoint t))
        (t nil)))

(defun arei--nrepl-port-string-to-number (s)
  "Converts `S' from string to number when suitable."
  (when (string-match "^\\([0-9]+\\)" s)
    (string-to-number (match-string 0 s))))

(defun arei--read-nrepl-port-from-file (file)
  "Attempts to read port from a file named by FILE.

Discards it if it can be determined that the port is not active."
  (when (file-exists-p file)
    (when-let* ((port-string (with-temp-buffer
                               (insert-file-contents file)
                               (buffer-string)))
                (port-number (arei--nrepl-port-string-to-number port-string)))
      (let ((lsof (executable-find "lsof")))
        (if (and lsof port-number)
            (unless (equal ""
                           (shell-command-to-string
                            (format "%s -i:%s" lsof port-number)))
              port-number)
          port-number)))))

(defun arei--try-find-nrepl-port-around ()
  "Try to find a `.nrepl-port' file and read a port number from
it. Start with a file in current directory, fallback to the one
in a project root.

Uses `arei--read-nrepl-port-from-file', so if there is `lsof' and
it shows that there is no process attached to the port, the port
will be ignored and function will try to find the next one.

Return nil if nothing found."
  (let ((possible-nrepl-port-files
         (list
          (expand-file-name ".nrepl-port")
          (when (project-current)
            (expand-file-name
             ".nrepl-port"
             (file-name-as-directory (project-root (project-current))))))))
    (seq-some
     (lambda (f)
       (when-let (port (and f (arei--read-nrepl-port-from-file f)))
         port))
     possible-nrepl-port-files)))

(defun arei--select-endpoint (params)
  "Read HOST and PORT from minibuffer and put them into plist."
  (let* ((nrepl-port-nearby (arei--try-find-nrepl-port-around))
         (default-host (or (plist-get params :host) "localhost"))
         (default-port (or (plist-get params :port) nrepl-port-nearby 7888)))
    (if (plist-get params :select-endpoint)
        (list
         :host
         (read-string (format "host (default %s): " default-host)
                      nil 'arei-host default-host)
         :port
         (read-number "port: " default-port 'arei-port))
      (list :host default-host :port default-port))))

(defun arei--start (arg)
  "Connect to remote endpoint using provided :HOST and :PORT
from PARAMS plist.  Initialize sesman session.  Read values from
minibuffer if called with prefix argument.  Users should not call
this function directly."
  (interactive "P")
  (let* ((params (thread-first
                   (arei--create-params-plist arg)
                   (arei--select-endpoint)))
         (host (plist-get params :host))
         (port (number-to-string (plist-get params :port)))
         (host-and-port (concat host ":" port))
         (project (project-current))
         (session-prefix (if project (project-name project) (buffer-name)))
         (sesman-session-name (concat session-prefix ":" host-and-port))
         ;; TODO: [Andrew Tropin, 2023-11-20] Handle the case when the
         ;; buffer already exists.
         (buffer-name (concat "*arei: " sesman-session-name "*")))

    ;; Prevent function being called directly, bypassing sesman
    ;; machinery and thus cleanup phaseses.
    (when (get-buffer buffer-name)
      (user-error "Connection buffer already exist."))

    (condition-case err
        (let* ((process (open-network-stream
                         (concat "nrepl-connection-" host-and-port)
                         buffer-name host port))
               (buffer (process-buffer process))
               (initial-buffer (current-buffer)))
          (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
          (set-process-filter process 'arei--connection-filter)
          (set-process-sentinel process 'arei--sentinel)
          (process-put process :string-q (queue-create))
          (process-put process :response-q (arei-nrepl-response-queue))

          (arei-client-clear-session-cache)
          (sesman-add-object 'Arei sesman-session-name buffer 'allow-new)

          (with-current-buffer buffer
            (arei-connection-mode)
            (setq arei-client--request-counter 0)
            (setq arei-client--nrepl-sessions (make-hash-table :test 'equal))
            (setq arei-client--pending-requests (make-hash-table :test 'equal))
            ;; Set the current working directory for the connection buffer
            ;; to the project root.
            (when (project-current)
              (setq default-directory (project-root (project-current))))

            (insert
             (propertize
              (format ";;; Connecting to nREPL host on '%s:%s'...\n" host port)
              'face
              '((t (:inherit font-lock-comment-face)))))
            (arei--initialize-sessions buffer initial-buffer))
          (when (fboundp arei-connection-buffer-display-function)
            (funcall arei-connection-buffer-display-function buffer))
          buffer)
        (error
         (progn
           (kill-buffer buffer-name)
           (message "%s" (error-message-string err)))))))


;;;
;;; arei-mode
;;;

(defun arei--modeline-connection-info ()
  (list
   "["
   (cond
    ((arei-spinner-modeline) (arei-spinner-modeline))
    ((arei-connected-p) "connected")
    (t "disconnected"))
   "]"))

(defvar arei-mode-line
  '(" arei" (:eval (arei--modeline-connection-info))) "\
Mode line lighter for arei mode.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for details
about mode line templates.

Customize this variable to change how arei mode displays its status in the
mode line.  The default value displays the current connection.  Set this
variable to nil to disable the mode line entirely.")

(put 'arei-mode-line 'risky-local-variable t)

(defvar-keymap arei-mode-map
  "C-c C-z" #'arei-switch-to-connection-buffer
  "C-c C-e" arei-evaluation-keymap
  "C-x C-e" #'arei-evaluate-last-sexp
  "C-c C-k" #'arei-evaluate-buffer
  "C-c C-i" #'arei-interrupt-evaluation
  "C-c M-m" arei-module-map
  "C-c C-m" arei-macroexpansion-map

  "C-M-x" #'arei-evaluate-top-level-form
  "C-c C-c" #'arei-evaluate-top-level-form)

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
        (add-hook 'eldoc-documentation-functions #'arei-eldoc-arglist nil t)
        (add-hook 'completion-at-point-functions #'arei-complete-at-point nil t)
        (add-hook 'xref-backend-functions #'arei--xref-backend nil t)
        (add-hook 'kill-buffer-hook #'arei-client-remove-from-session-cache nil t)
        (add-hook 'sesman-post-command-hook #'arei-client-clear-session-cache nil t)
        ;; (setq next-error-function #'arei-jump-to-compilation-error)
        )
    ;; Mode cleanup
    ;; (mapc #'kill-local-variable '(next-error-function))
    (remove-hook 'completion-at-point-functions #'arei-complete-at-point t)
    (remove-hook 'xref-backend-functions #'arei--xref-backend t)
    (remove-hook 'eldoc-documentation-functions #'arei-eldoc-arglist t)
    (remove-hook 'kill-buffer-hook #'arei-client-remove-from-session-cache t)
    (remove-hook 'sesman-post-command-hook #'arei-client-clear-session-cache t)))

;;;###autoload
(defun arei-mode--maybe-activate ()
  "Activates `arei-mode' if `arei-mode-auto' is t."
  (when arei-mode-auto
    (arei-mode)))

;;;###autoload
(defun arei ()
  "Connect to nrepl server."
  (interactive)
  (if (sesman-current-session 'Arei)
      (sesman-restart)
      (sesman-start)))

;;;###autoload
(defun arei--enable-on-existing-scheme-buffers ()
  "Enable Arei's minor mode on existing Scheme buffers.
See command `arei-mode'."
  (let ((scm-buffers (seq-filter
                      (lambda (buffer)
                        (with-current-buffer buffer
                          (derived-mode-p 'scheme-mode)))
                      (buffer-list))))
    (dolist (buffer scm-buffers)
      (with-current-buffer buffer
        (unless arei-mode
          (arei-mode--maybe-activate))))))

;;;###autoload
(with-eval-after-load 'scheme
  (keymap-set scheme-mode-map "C-c C-a" #'arei)
  (keymap-set scheme-mode-map "C-c C-s" 'sesman-map)
  (sesman-install-menu scheme-mode-map)
  (add-hook 'scheme-mode-hook 'arei-mode--maybe-activate)
  (arei--enable-on-existing-scheme-buffers))

;; TODO: Scratch buffer

(provide 'arei)
;;; arei.el ends here
