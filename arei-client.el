;;; arei-client.el --- Abstraction over nREPL interactions -*- lexical-binding: t; coding:utf-8 -*-

;; Copyright © 2023, 2024 Andrew Tropin
;; Copyright © 2024 Nikita Domnitskii

;; Author: Andrew Tropin <andrew@trop.in>
;;         Nikita Domnitskii <nikita@domnitskii.me>

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

;; Abstraction over nREPL interactions

;;; Code:

(require 'arei-nrepl)
(require 'sesman)
(require 'map)
(require 'project)

(defvar-local arei-client--request-counter 0
  "Serial number for message, used for association between request
and responses.")

(defvar-local arei-client--pending-requests nil
  "A hash-table containing callbacks for pending requests.")

(defvar-local arei-client--nrepl-sessions nil
  "A hash-table containing name and session-id association.")

(defvar arei-client-sync-timeout 5
  "Number of seconds to wait for a syncronous request's response.")

(defvar arei-client--sesman-session-cache (make-hash-table :test 'equal)
  "Session cache for `arei-connection-buffer'.")


;;;
;;; Sesman
;;;

(defun arei-client-remove-from-sesman-session-cache ()
  "Remove current file-name association from session cache.
This function is intended to be used as a value for `kill-buffer-hook'."
  (let ((filename (buffer-file-name (current-buffer))))
    (map-delete arei-client--sesman-session-cache filename)))

(defun arei-client-clear-sesman-session-cache ()
  "Clear session cache.
This function is intended to be used as a value for `sesman-post-command-hook'."
  ;; NOTE: no equalent in map.el
  (clrhash arei-client--sesman-session-cache))

(defun arei-connection-buffer ()
  "Return a connection buffer associated with the current session."
  (let ((filename (buffer-file-name (current-buffer))))
    (if (map-contains-key arei-client--sesman-session-cache filename)
        (map-elt arei-client--sesman-session-cache filename)
      (let ((buff (cadr (sesman-current-session 'Arei))))
        (map-put! arei-client--sesman-session-cache filename buff)))))

(defun arei-connected-p ()
  "Return t if Arei is currently connected, nil otherwise."
  (process-live-p (get-buffer-process (arei-connection-buffer))))

(defun arei-ensure-connection (connection)
  "Checks if CONNECTION is available, otherwise throws an error."
  (unless connection (error "No nREPL connection for current session")))

(defmacro arei-with-connection-buffer (&rest body)
  "Execute BODY in `arei-connection-buffer' context if it exists,
otherwise throw an error."
  (let ((con-buf-sym (make-symbol "connection-buffer")))
    `(let ((,con-buf-sym (arei-connection-buffer)))
       (if ,con-buf-sym
         (with-current-buffer ,con-buf-sym
           ,@body)
         (arei-ensure-connection nil)))))

;; MAYBE: Rename to switch-to-nrepl-session-buffer to make sure C-c
;; C-z jumps to buffer with needed output.
(defun arei-switch-to-connection-buffer ()
  (interactive)
  (if (arei-connection-buffer)
      (pop-to-buffer (arei-connection-buffer))
    (message "No connection associated with the buffer yet.")))


;;;
;;; nREPL Sessions
;;;

;; TODO: [Andrew Tropin, 2023-11-20] Add association between session
;; and output buffer for it.  It's needed to support multiple nrepl
;; sessions that can use separate buffers for stdin/stdout instead of
;; using primary connection buffer.  Also, adding
;; `arei-set-default-nrepl-session' may help for eval and switch
;; operations.
(defun arei-client--create-nrepl-session (session-name &optional callback)
  "Setups an nrepl session and register it in `arei-client--nrepl-sessions'."
  (let* ((response (arei-client-send-sync-request
                    (arei-nrepl-dict "op" "clone") nil))
         (new-session (arei-nrepl-dict-get response "new-session")))
    (if (not new-session)
        (error "nREPL session is not created.")
      (arei-with-connection-buffer
        (puthash session-name new-session arei-client--nrepl-sessions))
      (funcall callback session-name response)
      new-session)))

(defun arei-client--nrepl-session-creation-callback (session-name _response)
  "Display information about created session."
  (arei-with-connection-buffer
    (goto-char (point-max))
    (insert
     (propertize
      (format ";;; nREPL session created: %s\n" session-name)
      'face
      '((t (:inherit font-lock-comment-face)))))))

(defun arei-client--initialize-nrepl-sessions ()
  "Initialize sessions needed for Arei operation."
  (arei-client--create-nrepl-session
   "evaluation"
   'arei-client--nrepl-session-creation-callback)
  (arei-client--create-nrepl-session
   "tooling"
   'arei-client--nrepl-session-creation-callback))

(defun arei-client--get-session-id (name)
  "Get session-id for session NAME."
  (arei-with-connection-buffer
    (gethash name arei-client--nrepl-sessions)))

(defun arei--user-evaluation-session-id ()
  (arei-client--get-session-id "evaluation"))

(defun arei--tooling-session-id ()
  ;; Note: To avoid recursion, this wrapper can't be used in
  ;; `sesman-friendly-session-p', so be mindful if rename the session.
  ;; Don't forget to update it in `sesman-friendly-session-p' as well.
  (arei-client--get-session-id "tooling"))

(defun arei-client-ensure-session-id (name)
  "Get session-id for session NAME, create session if it doesn't
exist."
  (or (arei-client--get-session-id name)
      (arei-client--create-nrepl-session
       name
       'arei-client--nrepl-session-creation-callback)))


;;;
;;; arei-connection-mode
;;;

(defvar-keymap arei-connection-mode-map
  "C-c C-s" #'sesman-map)

;; (easy-menu-define arei-connection-mode-menu arei-connection-mode-map
;;   "Menu for Arei's connection mode"
;;   `("Arei"))

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

(defun arei-client--sentinel (process message)
  "Called when connection is changed; in out case dropped."
  (with-current-buffer (process-buffer process)
    (sesman-quit))
  (message "nREPL connection closed: %s" message)
  ;; NOTE: sesman-post-command-hook is not run when connection buffer is
  ;; killed, so we run it here just in case
  (run-hooks 'sesman-post-command-hook))

;; TODO: [Andrew Tropin, 2023-10-19] Handle incomplete incomming string.
(defun arei-client--connection-filter (process string)
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
              (arei-client--dispatch-response response))))))))

(defun arei-client--dispatch-response (response)
  "Find associated callback for a message by id."
  (pcase response
    ((map id status)
     (when-let* ((pending-request (gethash id arei-client--pending-requests)))
       (when-let (callback (cdr pending-request))
         (funcall callback response))
       (when (member "done" status)
           (remhash id arei-client--pending-requests))))))

(defun arei-client--connect (params)
  "Call callback after connection is established."
  (let* ((host (plist-get params :host))
         (port (number-to-string (plist-get params :port)))
         (host-and-port (concat host ":" port))
         (project (project-current))
         (sesman-session-prefix
          (if project (project-name project) (buffer-name)))
         (sesman-session-name
          (concat sesman-session-prefix ":" host-and-port))
         (buffer-name (concat "*arei: " sesman-session-name "*")))

    ;; Prevent function being called directly, bypassing sesman
    ;; machinery and thus cleanup phaseses.
    (when (get-buffer buffer-name)
      (user-error "Connection buffer already exist."))

    (condition-case err
        (let* ((process (open-network-stream
                         (concat "nrepl-connection-" host-and-port)
                         buffer-name host port))
               (buffer (process-buffer process)))
          (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
          (set-process-filter process 'arei-client--connection-filter)
          (set-process-sentinel process 'arei-client--sentinel)
          (process-put process :string-q (queue-create))
          (process-put process :response-q (arei-nrepl-response-queue))

          (arei-client-clear-sesman-session-cache)
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
            (arei-client--initialize-nrepl-sessions))
          (message "Connection to the nREPL server initialized.")
          buffer)
        (error
         (progn
           (kill-buffer buffer-name)
           (message "%s" (error-message-string err)))))))

(defun arei-client--print-pending-requests ()
  (interactive)
  (arei-with-connection-buffer
    (maphash (lambda (key value)
               (message "Key: %s, Value: %s" key value))
             arei-client--pending-requests)))


;;;
;;; Requests
;;;

(defun arei-client--send-request (request connection callback session-id)
  "Internal API for `arei-client-send-request', it should NOT be
 used directly."
  (arei-ensure-connection connection)
  (let ((id (arei-nrepl-dict-get request "id")))
    (unless id (error "No id provided for request"))
    (with-current-buffer connection
      (when session-id
        (arei-nrepl-dict-put request "session" session-id))
      (puthash id (cons request callback) arei-client--pending-requests)
      (process-send-string nil (arei-nrepl-bencode request)))))

(defun arei-client--send-sync-request
    (request connection session-id &optional timeout-callback)
  "Internal API for `arei-client-send-sync-request', it should
 NOT be used directly."
  (let ((time0 (current-time))
        response
        global-status)
    (arei-ensure-connection connection)
    (arei-client--send-request
     request
     connection
     (lambda (resp) (setq response resp))
     session-id)
    (while (not (member "done" global-status))
      (setq global-status (arei-nrepl-dict-get response "status"))
      (when (time-less-p arei-client-sync-timeout
                         (time-subtract nil time0))
        (if (functionp timeout-callback)
            (funcall timeout-callback request)
          (error "Sync nREPL request timed out %s" request)))
      (accept-process-output nil 0.01))
    response))

(defun arei-client--add-id-to-request (request)
  "Add id to request dict, id is based on the value of
`arei-client--request-counter'."
  (arei-with-connection-buffer
    (arei-nrepl-dict-put
     request
     "id"
     (number-to-string (cl-incf arei-client--request-counter)))))

(defun arei-client-send-request (request callback session-id)
  "Send REQUEST and assign CALLBACK.
The CALLBACK function will be called when reply is received.

SESSION-ID should be either session-id or nil.  nil is for
ephemeral session."
  (arei-client--add-id-to-request request)
  (arei-client--send-request
   request
   (arei-connection-buffer)
   callback
   session-id))

(defun arei-client-send-sync-request
    (request session-id &optional timeout-callback)
  "Send request to nREPL server synchronously."
  (arei-client--add-id-to-request request)
  (arei-client--send-sync-request
   request
   (arei-connection-buffer)
   session-id
   timeout-callback))

(provide 'arei-client)
;;; arei-client.el ends here
